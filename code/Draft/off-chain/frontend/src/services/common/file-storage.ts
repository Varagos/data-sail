import { readFile, writeFile } from 'fs/promises';

export class BaseFileKeyValueStorage<T> {
  constructor(protected readonly filePath: string) {}

  protected async readFromFile(): Promise<Map<string, T>> {
    const fileContent = await readFile(this.filePath, 'utf8');
    const jsonObject = JSON.parse(fileContent);
    return new Map(Object.entries(jsonObject));
  }

  protected async writeToFile(storage: Map<string, T>): Promise<void> {
    const jsonObject = Object.fromEntries(storage);
    const fileContent = JSON.stringify(jsonObject, null, 2);
    await writeFile(this.filePath, fileContent, 'utf8');
  }

  protected async addEntry(key: string, value: T): Promise<void> {
    const storage = await this.readFromFile();
    storage.set(key, value);
    await this.writeToFile(storage);
  }

  protected async retrieveEntry(key: string): Promise<T | null> {
    const storage = await this.readFromFile();
    const data = storage.get(key);
    if (!data) {
      return null;
    }
    return data;
  }

  protected async updateEntry(key: string, value: T): Promise<void> {
    const storage = await this.readFromFile();
    storage.set(key, value);
    await this.writeToFile(storage);
  }

  protected async removeEntry(key: string): Promise<void> {
    const storage = await this.readFromFile();
    storage.delete(key);
    await this.writeToFile(storage);
  }
}
