import { DataSession, StorageIdentifier } from '@/types';
import fs from 'fs/promises';
import path from 'path';
import { IStorage } from '.';

export class LocalJSONFileStorage implements IStorage {
  private readonly filePath: string;

  constructor(fileName: string) {
    // /workspace/code/Draft/off-chain/frontend/data/data.json
    const projectRoot = path.resolve(__dirname, '..', '..', '..', '..');
    // console.log({ projectRoot });
    const dataPath = path.resolve(projectRoot, 'data');
    this.filePath = path.resolve(dataPath, fileName);
  }

  public async deleteData(identifier: string): Promise<void> {
    const storage = await this.readFromFile();
    storage.delete(identifier);
    await this.writeToFile(storage);
  }

  public async storeData(data: string, id?: StorageIdentifier): Promise<StorageIdentifier> {
    const identifier = id || Date.now().toString();
    const storage = await this.readFromFile();
    storage.set(identifier, data);
    await this.writeToFile(storage);
    return identifier;
  }

  public async retrieveData(identifier: StorageIdentifier): Promise<string | null> {
    const storage = await this.readFromFile();
    const data = storage.get(identifier);
    if (!data) {
      return null;
    }
    return data;
  }

  private async readFromFile(): Promise<Map<StorageIdentifier, string>> {
    try {
      const fileContent = await fs.readFile(this.filePath, 'utf8');
      const jsonObject = JSON.parse(fileContent);
      return new Map(Object.entries(jsonObject));
    } catch (error) {
      return new Map();
    }
  }
  private async writeToFile(storage: Map<StorageIdentifier, string>): Promise<void> {
    const jsonObject = Object.fromEntries(storage);
    const fileContent = JSON.stringify(jsonObject, null, 2);
    await fs.writeFile(this.filePath, fileContent, 'utf8');
  }
}
