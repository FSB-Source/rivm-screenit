import type {RechtGeldigheid} from "../datatypes/RechtGeldigheid"
import {getDate, vandaagISO} from "./DateUtil"

export const isAuthorized = (recht: RechtGeldigheid): boolean => {
	return recht.authorized && (recht.eindDatum ? getDate(recht.eindDatum) >= vandaagISO() : true)
}