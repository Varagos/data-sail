import { ITokenListings, TokenListing } from './interface';
import { BaseRedisStorage } from '../common/redis';

export default class RedisTokenListings implements ITokenListings {
  private baseRedis: BaseRedisStorage;
  static PREFIX = 'token-listing';
  private getKeyName(tokenAssetClass: string) {
    return `${RedisTokenListings.PREFIX}:${tokenAssetClass}`;
  }

  constructor(baseRedis: BaseRedisStorage) {
    this.baseRedis = baseRedis;
  }

  async addTokenListing(tokenListing: TokenListing): Promise<void> {
    const value = JSON.stringify(tokenListing);
    const key = this.getKeyName(tokenListing.tokenAssetClass);
    await this.baseRedis.addEntry(key, value);
  }

  /**
   * Using a Redis Hash would be a better option here for performance reasons.
   */
  async fetchAllDataListings(): Promise<TokenListing[]> {
    const pattern = `${RedisTokenListings.PREFIX}:*`;
    const keys = await this.baseRedis.client.keys(pattern);

    const tokenListings: TokenListing[] = [];

    for (const key of keys) {
      const value = await this.baseRedis.client.get(key);
      if (!value) {
        console.error(`No value for key ${key}`);
        continue;
      }
      tokenListings.push(JSON.parse(value));
    }
    return tokenListings;
  }

  async removeTokenListing(tokenAssetClass: string): Promise<void> {
    const key = this.getKeyName(tokenAssetClass);
    return this.baseRedis.removeEntry(key);
  }
}
