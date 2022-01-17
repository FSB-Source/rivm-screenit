export type SessionActions = SetSessionAction | ClearSessionAction;

export const SET_SESSION = "SET_SESSION"
export type SetSessionAction = {
	type: "SET_SESSION";
	sessionId: string;
	gebruikersnaam: string;
	medewerkercode: string;
	displayName: string;
	seCode: string;
	seNaam: string;
	yubikeyIdentificatie: string;
	instellingGebruikerId: number;
};
export const createActionSetSession = (sessionId: string, gebruikersnaam: string, medewerkercode: string, displayName: string, seCode: string, seNaam: string, yubikeyIdentificatie: string, instellingGebruikerId: number): SetSessionAction => ({
	type: SET_SESSION,
	sessionId: sessionId,
	gebruikersnaam: gebruikersnaam,
	medewerkercode: medewerkercode,
	displayName: displayName,
	seCode: seCode,
	seNaam: seNaam,
	yubikeyIdentificatie: yubikeyIdentificatie,
	instellingGebruikerId: instellingGebruikerId,
})
export const CLEAR_SESSION = "CLEAR_SESSION"
export type ClearSessionAction = {
	type: "CLEAR_SESSION";
};
export const createActionClearSession = (): ClearSessionAction => ({
	type: CLEAR_SESSION,
})