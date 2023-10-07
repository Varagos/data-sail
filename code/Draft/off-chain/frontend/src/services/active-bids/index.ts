import ActiveBidsFileStorage from './active-bids';
import { IActiveBids } from './interface';

export const activeBidsStorageService: IActiveBids = new ActiveBidsFileStorage();
