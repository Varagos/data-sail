import { resolve } from 'path';
import { BaseFileKeyValueStorage } from '../common/file-storage';
import { IActiveBids, ActiveBid } from './interface';

export default class ActiveBidsFileStorage extends BaseFileKeyValueStorage<string> implements IActiveBids {
  constructor(fileName: string = 'active-bids.json') {
    const dataPath = '/workspace/code/Draft/off-chain/frontend/data';
    const filePath = resolve(dataPath, fileName);
    super(filePath);
  }

  async addActiveBid(wallet: string, activeBid: ActiveBid): Promise<void> {
    const value = JSON.stringify(activeBid);
    const existingSerialized = await this.retrieveEntry(wallet);
    const existing = existingSerialized ? JSON.parse(existingSerialized) : [];
    existing.push(value);
    const newValue = JSON.stringify(existing);
    await this.addEntry(wallet, newValue);
  }

  async fetchAllActiveBids(wallet: string): Promise<ActiveBid[]> {
    const storage = await this.retrieveEntry(wallet);
    if (!storage) {
      return [];
    }
    const activeBids = JSON.parse(storage);
    return activeBids;
  }

  async removeActiveBid(wallet: string, bidId: string): Promise<void> {
    const existingSerialized = await this.retrieveEntry(wallet);
    const existing = existingSerialized ? JSON.parse(existingSerialized) : [];
    const filtered = existing.filter((bid: ActiveBid) => bid.id !== bidId);
    const newValue = JSON.stringify(filtered);
    await this.updateEntry(wallet, newValue);
  }
}
