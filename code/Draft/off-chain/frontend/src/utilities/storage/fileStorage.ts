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

  public async storeData(data: DataSession | string, id?: StorageIdentifier): Promise<StorageIdentifier> {
    const identifier = id || Date.now().toString();
    const storage = await this.readFromFile();
    storage.set(identifier, data);
    await this.writeToFile(storage);
    return identifier;
  }

  public async retrieveData(identifier: StorageIdentifier): Promise<DataSession | string | null> {
    const storage = await this.readFromFile();
    const data = storage.get(identifier);
    if (!data) {
      return null;
    }
    return data;
  }

  public async retrieveAllData(): Promise<Array<DataSession | string>> {
    const storage = await this.readFromFile();
    // Here we ensure that the first item is always the most recent one for simplicity
    const sortedData = Array.from(storage.entries())
      .sort((a, b) => Number(b[0]) - Number(a[0])) // Sort by keys (timestamps) in descending order
      .map((entry) => entry[1]); // Extract the values
    return sortedData;
    // return Array.from(storage.values());
  }

  private async readFromFile(): Promise<Map<StorageIdentifier, DataSession | string>> {
    try {
      const fileContent = await fs.readFile(this.filePath, 'utf8');
      const jsonObject = JSON.parse(fileContent);
      return new Map(Object.entries(jsonObject));
    } catch (error) {
      return new Map();
    }
  }
  private async writeToFile(storage: Map<StorageIdentifier, DataSession | string>): Promise<void> {
    const jsonObject = Object.fromEntries(storage);
    const fileContent = JSON.stringify(jsonObject, null, 2);
    await fs.writeFile(this.filePath, fileContent, 'utf8');
  }
}
