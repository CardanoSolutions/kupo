#!/usr/bin/env node

const fs = require('fs');
const http = require('http');
const process = require('process');
const cp = require('child_process');
const assert = require('assert');
const pidUsage = require('pidusage');
const path = require('path');

const DEFAULT_URL = 'http://localhost:1442';

const SINCE = "origin";
const UNTIL =  64361263;

main().then(result => console.error(JSON.stringify(result)))

async function main() {
  const tmp = fs.mkdtempSync('kupo-synchronize-alonzo');
  console.error(`bench (${tmp}): ${process.argv.slice(3)}`);
  let kupo;
  try {
    const cmd = process.argv[2];
    const extraOptions = process.argv.slice(3)
    assert.ok(cmd, 'Missing mandatory argument <binary-filepath>');

    const { configFile, socketFile } = findNodeConfigurationSync();

    const now = Date.now();

    kupo = cp.spawn(cmd,
      [ '--node-socket', socketFile
      , '--node-config', configFile
      , '--match', '*'
      , '--workdir', tmp
      , '--since', SINCE
      ].concat(extraOptions));

    kupo.stdout.on('data', chunk => process.stdout.write(chunk.toString()));
    kupo.stderr.on('data', chunk => process.stderr.write(chunk.toString()));
    kupo.on('close', code => process.exit(code));

    const memoryStream = fs.createWriteStream(path.join(tmp, 'memory.log'));

    let memoryUsage = 0;
    let ix = 0;
    const watchMemory = setInterval(() => {
      pidUsage(kupo.pid, (err, stats) => {
        memoryStream.write(ix + ' ' + stats.memory + '\n');
        if (stats.memory > memoryUsage)  {
          memoryUsage = stats.memory;
        }
        ix += 1;
      });
    }, 2000);

    await waitForSlot(UNTIL);
    memoryStream.close();
    clearInterval(watchMemory);

    const databaseSize = cp.execSync(`ls -lh ${tmp}/kupo.sqlite3`).toString().split(" ")[4];
    const maxMemory = `${memoryUsage / (1024 * 1024)}M`;

    const time = (Date.now() - now) / 1000;
    if (time < 1000) {
      return { duration: `${time}s`, databaseSize, maxMemory };
    } else if (time < 3600 * 3) {
      return { duration: `${time / 60}min`, databaseSize, maxMemory };
    } else {
      const hour = Math.floor(time / 3600);
      const min = time - hour * 3600;
      return { duration: `${hour}h${min}min`, databaseSize, maxMemory };
    }
  } catch (e) {
    fs.rmSync(tmp, { recursive: true });
    throw e;
  } finally {
    if (kupo) {
      process.kill(kupo.pid);
    }
  }
}

function findNodeConfigurationSync() {
  const out = cp.execSync(`ps -aux | grep 'cardano-node run'`).toString().split('\n')[0];
  const configFile = out.replace(/.*--config ([^ ]*) ?.*/gm, "$1").trim();
  const socketFile = out.replace(/.*--socket-path ([^ ]*) ?.*/gm, "$1").trim();

  assert.ok(configFile, "Unable to infer cardano-node config's file. Is the cardano-node process running?")
  assert.ok(socketFile, "Unable to infer cardano-node socket's file. Is the cardano-node process running?")

  return { configFile, socketFile }
}

async function waitForSlot(target) {
  const POLL_DELAY = 5000; // ms

  return new Promise((resolve) => {
    function poll() {
      http.get(`${DEFAULT_URL}/v1/health`, res => {
        res.setEncoding('utf8');
        let body = '';
        res.on('data', (chunk) => { body += chunk; });
        res.on('end', () => {
          const { most_recent_checkpoint } = JSON.parse(body);
          if (Number(most_recent_checkpoint) >= target) {
            resolve();
          } else {
            setTimeout(poll, POLL_DELAY);
          }
        })
      });
    }

    setTimeout(poll, POLL_DELAY);
  });
}
