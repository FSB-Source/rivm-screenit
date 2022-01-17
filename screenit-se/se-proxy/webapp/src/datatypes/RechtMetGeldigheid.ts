import type {RechtGeldigheid} from "./RechtGeldigheid"

export type RechtMetGeldigheid = {
	readonly inschrijvenRecht: RechtGeldigheid;
	readonly onderzoekenRecht: RechtGeldigheid;
	readonly signalerenRecht: RechtGeldigheid;
	readonly kwaliteitsopnameRecht: RechtGeldigheid;
	readonly connectiestatusRecht: RechtGeldigheid;
};