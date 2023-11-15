import { BaseRedisStorage } from '../common/redis';
import ActiveBidsFileStorage from './active-bids';
import { IActiveBids } from './interface';
import RedisActiveBids from './redis-active-bids';

// export const activeBidsStorageService: IActiveBids = new ActiveBidsFileStorage();

const baseRedisStorage = await BaseRedisStorage.getInstance();
const redisActiveBids = new RedisActiveBids(baseRedisStorage);
export { redisActiveBids };
