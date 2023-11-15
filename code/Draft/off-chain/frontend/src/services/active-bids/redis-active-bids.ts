import { IActiveBids, ActiveBid } from './interface';
import { BaseRedisStorage } from '../common/redis';

export default class RedisActiveBids implements IActiveBids {
  private baseRedis: BaseRedisStorage;
  static PREFIX = 'active-bids';

  private getKeyName(wallet: string) {
    return `${RedisActiveBids.PREFIX}:${wallet}`;
  }

  constructor(baseRedis: BaseRedisStorage) {
    this.baseRedis = baseRedis;
  }

  async addActiveBid(wallet: string, activeBid: ActiveBid): Promise<void> {
    const key = this.getKeyName(wallet);
    const existingSerialized = await this.baseRedis.retrieveEntry(key);

    const existing: ActiveBid[] = existingSerialized ? JSON.parse(existingSerialized) : [];
    existing.push(activeBid);
    const newValue = JSON.stringify(existing);
    await this.baseRedis.addEntry(key, newValue);
  }

  async fetchAllActiveBids(wallet: string): Promise<ActiveBid[]> {
    const key = this.getKeyName(wallet);
    // console.log('Redis fetching active bids for key', key);
    const storage = await this.baseRedis.retrieveEntry(key);
    // console.log('Redis storage', storage);
    if (!storage) {
      return [];
    }
    const activeBids = JSON.parse(storage);
    // console.log('Redis active bids', activeBids);
    return activeBids;
  }

  async removeActiveBid(wallet: string, bidId: string): Promise<void> {
    console.log('Removing active bid', wallet, bidId);
    const key = this.getKeyName(wallet);
    const existingSerialized = await this.baseRedis.retrieveEntry(key);
    const existing: ActiveBid[] = existingSerialized ? JSON.parse(existingSerialized) : [];
    const filtered = existing.filter((bid: ActiveBid) => bid.id !== bidId);
    const newValue = JSON.stringify(filtered);
    await this.baseRedis.updateEntry(key, newValue);
  }
}
