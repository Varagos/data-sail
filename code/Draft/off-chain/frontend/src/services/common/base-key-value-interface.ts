export interface IBaseFileKeyValueStorage<T> {
  addEntry(key: string, value: T): Promise<void>;

  retrieveEntry(key: string): Promise<T | null>;

  updateEntry(key: string, value: T): Promise<void>;

  removeEntry(key: string): Promise<void>;
}
