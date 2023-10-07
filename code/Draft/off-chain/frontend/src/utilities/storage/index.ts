import { DataSession, StorageIdentifier } from '@/types';
import { LocalJSONFileStorage } from './fileStorage';
import { IpfsStorage } from './ipfsStorage';

export interface IStorage {
  storeData(data: DataSession | string, id?: StorageIdentifier): Promise<StorageIdentifier>;
  retrieveData(identifier: StorageIdentifier): Promise<DataSession | string | null>;
  retrieveAllData(): Promise<Array<DataSession | string>>;
  deleteData(identifier: StorageIdentifier): Promise<void>;
}

const storage = new LocalJSONFileStorage('data.json');
const ipfsStorage = new IpfsStorage();
export { storage, ipfsStorage };

// const storage = new InMemoryFileStorage();
// export { storage };
