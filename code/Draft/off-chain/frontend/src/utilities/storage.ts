type StorageIdentifier = string;
type StorageData = string;

interface IStorage {
  storeData(data: string): Promise<StorageIdentifier>;
  retrieveData(identifier: StorageIdentifier): Promise<string>;
}

class InMemoryFileStorage implements IStorage {
  private storage: Map<StorageIdentifier, string> = new Map();

  public async storeData(data: string): Promise<StorageIdentifier> {
    const identifier = Date.now().toString();
    this.storage.set(identifier, data);
    return identifier;
  }

  public async retrieveData(identifier: StorageIdentifier): Promise<StorageData> {
    const data = this.storage.get(identifier);
    if (!data) {
      throw new Error(`Data with identifier ${identifier} not found`);
    }
    return data;
  }
}

const storage = new InMemoryFileStorage();
export { storage };
