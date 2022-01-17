import type {ConnectieStatusLevel} from "../../datatypes/connectiestatus/ConnectieStatus"

export interface ConnectieStatusItem {
	getStatusLevel: () => ConnectieStatusLevel;
}