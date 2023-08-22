import { DataSession, StorageIdentifier } from '@/types';
import fs from 'fs/promises';
import path from 'path';

interface IStorage {
  storeData(data: DataSession): Promise<StorageIdentifier>;
  retrieveData(identifier: StorageIdentifier): Promise<DataSession>;
  retrieveAllData(): Promise<DataSession[]>;
}

class InMemoryFileStorage implements IStorage {
  private storage: Map<StorageIdentifier, DataSession> = new Map();

  public async storeData(data: DataSession): Promise<StorageIdentifier> {
    const identifier = Date.now().toString();
    this.storage.set(identifier, data);
    return identifier;
  }

  public async retrieveData(identifier: StorageIdentifier): Promise<DataSession> {
    const data = this.storage.get(identifier);
    if (!data) {
      throw new Error(`Data with identifier ${identifier} not found`);
    }
    return data;
  }

  public async retrieveAllData(): Promise<DataSession[]> {
    return Array.from(this.storage.values());
  }
}

class LocalJSONFileStorage implements IStorage {
  private readonly filePath: string;

  constructor(fileName: string) {
    this.filePath = path.resolve(__dirname, fileName);
  }

  private async readFromFile(): Promise<Map<StorageIdentifier, DataSession>> {
    try {
      const fileContent = await fs.readFile(this.filePath, 'utf8');
      const jsonObject = JSON.parse(fileContent);
      return new Map(Object.entries(jsonObject));
    } catch (error) {
      return new Map();
    }
  }

  public async storeData(data: DataSession): Promise<StorageIdentifier> {
    const identifier = Date.now().toString();
    const storage = await this.readFromFile();
    storage.set(identifier, data);
    await this.writeToFile(storage);
    return identifier;
  }
  private async writeToFile(storage: Map<StorageIdentifier, DataSession>): Promise<void> {
    const jsonObject = Object.fromEntries(storage);
    const fileContent = JSON.stringify(jsonObject, null, 2);
    await fs.writeFile(this.filePath, fileContent, 'utf8');
  }

  public async retrieveData(identifier: StorageIdentifier): Promise<DataSession> {
    const storage = await this.readFromFile();
    const data = storage.get(identifier);
    if (!data) {
      throw new Error(`Data with identifier ${identifier} not found`);
    }
    return data;
  }

  public async retrieveAllData(): Promise<DataSession[]> {
    const storage = await this.readFromFile();
    // Here we ensure that the first item is always the most recent one for simplicity
    const sortedData = Array.from(storage.entries())
      .sort((a, b) => Number(b[0]) - Number(a[0])) // Sort by keys (timestamps) in descending order
      .map((entry) => entry[1]); // Extract the values
    return sortedData;
    // return Array.from(storage.values());
  }
}

const storage = new LocalJSONFileStorage('data.json');
export { storage };

// const storage = new InMemoryFileStorage();
// export { storage };
