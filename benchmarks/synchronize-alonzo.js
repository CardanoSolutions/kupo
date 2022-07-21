#!/usr/bin/env node

const fs = require('fs');
const http = require('http');
const process = require('process');
const cp = require('child_process');
const assert = require('assert');

const DEFAULT_URL = 'http://localhost:1442';
const LAST_MARY_POINT = '36158304.2b95ce628d36c3f8f37a32c2942b48e4f9295ccfe8190bcbc1f012e1e97c79eb';
const LAST_ALONZO_SLOT = 62510369;

main().then(console.log);

async function main() {
  const tmp = fs.mkdtempSync('kupo-synchronize-alonzo');
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
      , '--match', '*/*'
      , '--workdir', tmp
      , '--since', LAST_MARY_POINT
      ].concat(extraOptions));

    kupo.stdout.on('data', chunk => console.log(chunk.toString()));
    kupo.stderr.on('data', chunk => console.log(chunk.toString()));
    kupo.on('close', code => process.exit(code));

    await waitForSlot(LAST_ALONZO_SLOT);

    const time = (Date.now() - now) / 1000;
    if (time < 1000) {
      return `${time}s`;
    } else if (time < 3600 * 3) {
      return `${time / 60}min`;
    } else {
      const hour = Math.floor(time / 3600);
      const min = time - hour * 3600;
      return `${hour}h${min}min`;
    }

  } finally {
    fs.rmSync(tmp, { recursive: true });
    if (kupo) {
      process.kill(kupo.pid);
    }
  }
}

function findNodeConfigurationSync() {
  const out = cp.execSync(`ps -aux | grep 'cardano-node'`).toString().split('\n')[0];
  const configFile = out.replace(/.*--config ([^ ]*) ?.*/gm, "$1").trim();
  const socketFile = out.replace(/.*--socket-path ([^ ]*) ?.*/gm, "$1").trim();

  assert.ok(configFile, "Unable to infer cardano-node config's file. Is the cardano-node process running?")
  assert.ok(socketFile, "Unable to infer cardano-node socket's file. Is the cardano-node process running?")

  return { configFile, socketFile }
}

async function waitForSlot(target) {
  const POLL_DELAY = 20000; // ms

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
