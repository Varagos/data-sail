import { DataSession, StorageIdentifier } from '@/types';
import { LocalJSONFileStorage } from './fileStorage';

export interface IStorage {
  storeData(data: DataSession): Promise<StorageIdentifier>;
  retrieveData(identifier: StorageIdentifier): Promise<DataSession>;
  retrieveAllData(): Promise<DataSession[]>;
}

const storage = new LocalJSONFileStorage('data.json');
export { storage };

// const storage = new InMemoryFileStorage();
// export { storage };
