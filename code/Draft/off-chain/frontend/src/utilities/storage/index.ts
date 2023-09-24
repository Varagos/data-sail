import { DataSession, StorageIdentifier } from '@/types';
import { LocalJSONFileStorage } from './fileStorage';

export interface IStorage {
  storeData(data: DataSession | string, id?: StorageIdentifier): Promise<StorageIdentifier>;
  retrieveData(identifier: StorageIdentifier): Promise<DataSession | string | null>;
  retrieveAllData(): Promise<Array<DataSession | string>>;
}

const storage = new LocalJSONFileStorage('data.json');
export { storage };

// const storage = new InMemoryFileStorage();
// export { storage };
