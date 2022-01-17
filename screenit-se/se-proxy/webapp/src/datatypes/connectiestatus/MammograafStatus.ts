import type {MammograafDicomMessageError} from "./MammograafDicomMessageError"

export type MammograafStatus = {
	aeTitle: string;
	laatsteSuccesDmwlBerichtTimestamp?: string;
	foutenSindsLaatsteSuccesDmwlBericht: Array<MammograafDicomMessageError>;
	laatsteSuccesMppsBerichtTimestamp?: string;
	foutenSindsLaatsteSuccesMppsBericht: Array<MammograafDicomMessageError>;
	mammograafDatum?: string;
};