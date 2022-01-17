export type PassantAfspraakMakenActions = AfspraakMakenPassantAction;
export const AFSPRAAK_MAKEN_PASSANT = "AFSPRAAK_MAKEN_PASSANT"
export type AfspraakMakenPassantAction = {
	type: "AFSPRAAK_MAKEN_PASSANT";
	geboortedatum: string;
	bsn: string;
};
export const createActionAfspraakMakenPassant = (geboortedatum: string, bsn: string): AfspraakMakenPassantAction => {
	return {
		type: AFSPRAAK_MAKEN_PASSANT,
		geboortedatum: geboortedatum,
		bsn: bsn,
	}
}