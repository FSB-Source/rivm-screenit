export type SeGebruikersActions = AddAllSeGebruikersAction | AddSeGebruikerAction | ClearSeGebruikersAction;
export const ADD_ALL_SE_GEBRUIKERS = "ADD_ALL_SE_GEBRUIKERS"
export type AddAllSeGebruikersAction = {
	type: "ADD_ALL_SE_GEBRUIKERS";
	seGebruikers: Map<number, string>;
};
export const createActionAddAllSeGebruikers = (seGebruikers: Map<number, string>): AddAllSeGebruikersAction => {
	return {
		type: ADD_ALL_SE_GEBRUIKERS,
		seGebruikers: seGebruikers,
	}
}
export const ADD_SE_GEBRUIKER = "ADD_SE_GEBRUIKER"
export type AddSeGebruikerAction = {
	type: "ADD_SE_GEBRUIKER";
	instellingGebruikerId: string;
	displayName: string;
};
export const createActionAddSeGebruiker = (instellingGebruikerId: string, displayName: string): AddSeGebruikerAction => {
	return {
		type: ADD_SE_GEBRUIKER,
		instellingGebruikerId: instellingGebruikerId,
		displayName: displayName,
	}
}
export const CLEAR_SE_GEBRUIKERS = "CLEAR_SE_GEBRUIKERS"
export type ClearSeGebruikersAction = {
	type: "CLEAR_SE_GEBRUIKERS";
};
export const createActionClearSeGebruikers = (): ClearSeGebruikersAction => {
	return {
		type: CLEAR_SE_GEBRUIKERS,
	}
}