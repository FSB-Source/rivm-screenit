export type Tab =
	"Daglijst"
	| "Cli\xEBntgegevens"
	| "Onderzoek"
	| "Dagverslag"
	| "Kwaliteitsopname"
	| "Connectiestatus"
	| "Geen";
export type SubPagina = "Vorige onderzoeken" | "Visuele inspectie" | "Signaleren";
export type NavigationState = {
	readonly tab: Tab;
	readonly subPagina?: SubPagina;
	readonly clientId?: number;
	readonly afspraakId?: number;
};
export const isDaglijstTab = (tab: Tab): boolean => {
	return tab === ("Daglijst" as Tab)
}
export const isClientgegevensTab = (tab: Tab): boolean => {
	return tab === ("Cliëntgegevens" as Tab)
}
export const isOnderzoeksTab = (tab: Tab): boolean => {
	return tab === ("Onderzoek" as Tab)
}
export const isDagverslagTab = (tab: Tab): boolean => {
	return tab === ("Dagverslag" as Tab)
}
export const isKwaliteitsopnameTab = (tab: Tab): boolean => {
	return tab === ("Kwaliteitsopname" as Tab)
}
export const isConnectiestatusTab = (tab: Tab): boolean => {
	return tab === ("Connectiestatus" as Tab)
}