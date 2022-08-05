#!/usr/bin/env node

const assert = require('assert');
const fs = require('fs');
const path = require('path');
const pidUsage = require('pidusage');
const process = require('process');

async function main() {
  const pid = Number(process.argv[2]);
  assert.ok(!Number.isNaN(pid) && pid != null, 'Missing mandatory argument <PID>');

  const workdir = process.argv[3];
  assert.ok(workdir, 'Missing mandatory argument <DIR>');

  const memoryStream = fs.createWriteStream(path.join(workdir, 'memory.log'));

  let maxMemoryInBytes = 0;
  let watchMemory;
  try {
    let ix = 0;
    await new Promise((resolve, reject) => {
      watchMemory = setInterval(() => {
        pidUsage(pid, (err, stats) => {
          if (err) { return reject(err); }
          memoryStream.write(ix + ' ' + stats.memory + '\n');
          if (stats.memory > maxMemoryInBytes)  {
            maxMemoryInBytes = stats.memory;
          }
          ix += 1;
        });
      }, 1000);
    });
  } finally {
    memoryStream.close();
    const maxMemory = Math.floor(maxMemoryInBytes / (1024 * 1024)) + "MB";
    clearInterval(watchMemory);
    console.log({ maxMemory });
  }
}

main();
