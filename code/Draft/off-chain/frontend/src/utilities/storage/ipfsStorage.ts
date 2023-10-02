import { BlockFrostIPFS } from '@blockfrost/blockfrost-js';
import Crypto from 'crypto';
import { tmpdir } from 'os';
import { join } from 'path';
import { IStorage } from './index';
import { DataSession, StorageIdentifier } from '@/types';
import { unlink, writeFile } from 'fs/promises';

export class IpfsStorage implements IStorage {
  private IPFS = new BlockFrostIPFS({
    projectId: 'ipfsgAnaNsw3s2FUF7sXNKCwQQKqQ6q54UIZ',
  });

  private ipfsProjectId: string;

  constructor() {
    const ipfsProjectId = process.env.BLOCKFROST_IPFS_KEY;
    if (!ipfsProjectId) {
      throw new Error('Missing BLOCKFROST_IPFS_KEY environment variable');
    }
    this.ipfsProjectId = ipfsProjectId;
    this.IPFS = new BlockFrostIPFS({
      projectId: ipfsProjectId,
    });
  }

  public async deleteData(identifier: string): Promise<void> {
    await this.IPFS.pinRemove(identifier);
  }

  public async storeData(data: DataSession | string): Promise<StorageIdentifier> {
    let tmpFilePath: string = '';
    try {
      // Create temp file
      const ext = typeof data === 'string' ? 'txt' : 'json';
      tmpFilePath = this.tmpFile(ext);
      const fileContent = typeof data === 'string' ? data : JSON.stringify(data, null, 2);
      await writeFile(tmpFilePath, fileContent);

      const added = await this.IPFS.add(tmpFilePath);
      console.log('added', added);

      const pinned = await this.IPFS.pin(added.ipfs_hash);
      const cid = pinned.ipfs_hash;
      console.log('pinned', pinned);
      return cid;
    } catch (err) {
      console.log('error', err);
      throw err;
    } finally {
      if (tmpFilePath) {
        // Delete temp file
        await unlink(tmpFilePath);
      }
    }
  }

  public async retrieveData(cid: StorageIdentifier): Promise<DataSession | string | null> {
    const url = `https://ipfs.blockfrost.io/api/v0/ipfs/gateway/${cid}`;
    const response = await fetch(url, {
      headers: {
        project_id: this.ipfsProjectId,
      },
    });
    const data = await response.text();
    console.log('read ipfs data', data);
    return data;
  }

  public async retrieveAllData(): Promise<Array<DataSession | string>> {
    throw new Error('Method not implemented.');
  }

  private tmpFile(ext: string) {
    return join(tmpdir(), `archive.${Crypto.randomBytes(6).readUIntLE(0, 6).toString(36)}.${ext}`);
  }
}
