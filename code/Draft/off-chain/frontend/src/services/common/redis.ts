import { createClient, RedisClientType } from 'redis';
import { IBaseFileKeyValueStorage } from './base-key-value-interface';

export class BaseRedisStorage implements IBaseFileKeyValueStorage<string> {
  private static instance: BaseRedisStorage | null = null;
  public client: ReturnType<typeof createClient>;

  private constructor(client: ReturnType<typeof createClient>) {
    this.client = client;
  }

  public static async getInstance(): Promise<BaseRedisStorage> {
    if (this.instance === null) {
      console.log('Connecting to Redis Client...');
      const client = await createClient({
        url: 'redis://redis:6379',
      })
        .on('error', (err) => console.log('Redis Client Error', err))
        .connect();
      console.log('Redis Client Connected!');

      this.instance = new BaseRedisStorage(client);
    }
    return this.instance;
  }

  async addEntry(key: string, value: string): Promise<void> {
    await this.client.set(key, value);
  }

  async retrieveEntry(key: string): Promise<string | null> {
    return this.client.get('key');
  }

  async updateEntry(key: string, value: string): Promise<void> {
    await this.client.set(key, value);
  }

  async removeEntry(key: string): Promise<void> {
    await this.client.del(key);
  }
}

// async function run() {
//   const storage = await BaseRedisStorage.getInstance();
//   // Now you can use the `storage` instance
//   const anotherStorage = await BaseRedisStorage.getInstance(); // Will be the same instance
// }

// run();
