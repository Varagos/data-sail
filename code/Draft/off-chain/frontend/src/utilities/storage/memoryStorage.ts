import { DataSession, StorageIdentifier } from '@/types';
import { IStorage } from '.';

export class InMemoryFileStorage implements IStorage {
  private storage: Map<StorageIdentifier, DataSession> = new Map();

  public async storeData(data: DataSession): Promise<StorageIdentifier> {
    const identifier = Date.now().toString();
    // Store to .json file here instead of in memory
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

  public async deleteData(identifier: string): Promise<void> {
    this.storage.delete(identifier);
  }
}
