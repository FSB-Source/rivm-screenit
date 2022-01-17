export const connectieStatusLevels = {
	OK: "fa fa-check-circle",
	WARN: "fa fa-exclamation-triangle",
	FAULT: "fa fa-exclamation-circle",
}
export const getMostCriticalStatusLevel = (connectieStatusses: Array<ConnectieStatusLevel>): ConnectieStatusLevel => {
	const highestStatusIndex = Math.max(...connectieStatusses.map(statusType => Object.keys(connectieStatusLevels).findIndex(c => c === statusType)))
	return Object.keys(connectieStatusLevels)[highestStatusIndex] as ConnectieStatusLevel
}
export type ConnectieStatusLevel = keyof typeof connectieStatusLevels;
export type ConnectieStatus = {
	mammograafConnectieStatusByAeTitle: Map<string, ConnectieStatusLevel>;
	imsConnectieStatus: ConnectieStatusLevel;
	imsConnectieStatusTimestamp?: string;
};