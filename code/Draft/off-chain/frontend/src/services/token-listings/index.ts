import { BaseRedisStorage } from '../common/redis';
import FileTokenListings from './file-token-listings';
import { ITokenListings } from './interface';
import RedisTokenListings from './redis-token-listings';

export const tokenListingStorageService: ITokenListings = new FileTokenListings();

const baseRedisStorage = await BaseRedisStorage.getInstance();
const redisTokenListings = new RedisTokenListings(baseRedisStorage);
export { redisTokenListings };
