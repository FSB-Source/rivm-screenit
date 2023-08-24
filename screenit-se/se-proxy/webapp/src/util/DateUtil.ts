import moment, {Duration} from "moment"
import localization from "moment/locale/nl"
import {createActionKiesDaglijstDatum} from "../actions/DaglijstDatumActions"
import {store} from "../Store"
import {leesAfspraken} from "../restclient/DaglijstRestclient"
import {leesPlanning} from "../restclient/PlanningRestClient"
import {createActionNavigateToDaglijst} from "../actions/NavigationActions"

moment.updateLocale("nl", localization)
export const vandaagISO = (): string => nu().format("YYYY-MM-DD")
let offset = 0

export const setOffset = (val: Duration): void => {
	offset = Number(val)

	if (store.getState().session) {
		store.dispatch(createActionKiesDaglijstDatum(vandaagISO()))
		leesAfspraken(vandaagISO(), createActionNavigateToDaglijst())
		leesPlanning(vandaagISO())
	}
}

export const nu = (): moment.Moment => {
	if (offset !== 0) {
		const nuDST = moment().isDST()
		const goal = moment().add(offset)
		const goalDST = goal.isDST()

		if (nuDST && !goalDST) {
			goal.add(moment.duration({
				"s": 3600,
			}))
		} else if (!nuDST && goalDST) {
			goal.subtract(moment.duration({
				"s": 3600,
			}))
		}

		return goal
	}

	return moment()
}

export const nuISO = (): string => nu().format("YYYY-MM-DDTHH:mm:ss")

export const nuTimestamp = (): string => nu().format("HH:mm:ss.SSS")

export const nuTijdUrenMinuten = (): string => nu().format("HH:mm")

export const ligtTussenData = (datum: Date, startDatum: Date | null, eindDatum: Date | null): boolean => {
	if (startDatum || eindDatum) {
		return (startDatum ? getDate(String(startDatum)) <= String(datum) : true) && (eindDatum ? String(datum) <= getDate(String(eindDatum)) : true)
	} else {
		return false
	}
}

export const datumFormaat = (isoDatum: string | Date | null | undefined): string => isoDatum ? moment(String(isoDatum)).format("DD-MM-YYYY") : ""

export const tijdFormaat = (isoTijd: string | undefined): string => isoTijd ? moment(isoTijd).format("HH:mm") : ""

export const datumTijdFormaat = (isoTijd: string | undefined): string => isoTijd ? moment(isoTijd).format("DD-MM-YYYY HH:mm") : ""

export const getDate = (isoDatetime: string): string => {
	return isoDatetime.split("T")[0]
}

export const getTime = (isoDatetime: string): string => {
	return isoDatetime.split("T")[1].slice(0, 5)
}

export const datumInVerleden = (isoDateTime: string): boolean => {
	const datum = new Date(isoDateTime)
	const gisteren = nu().subtract(1, "days").toDate()
	return datum <= gisteren
}

export const datumInToekomst = (isoDateTime: string): boolean => {
	const datum = new Date(isoDateTime)
	const vandaagIso = nu().toDate()
	return datum > vandaagIso
}

export const isAfter = (vergelijkingsIsoDatum: string, referentieIsoDatum: string): boolean => {
	const vergelijkingsDatum = new Date(vergelijkingsIsoDatum)
	const referentieDatum = new Date(referentieIsoDatum)
	return vergelijkingsDatum > referentieDatum
}

export const getTijdGeledenTekst = (timestamp: string): string => {
	return moment(timestamp).fromNow()
}

export const vandaagPlusDagen = (aantal: number): string => {
	return nu().add(aantal, "d").format("YYYY-MM-DDTHH:mm:ss")
}

export const vandaagDate = (): Date => {
	return new Date(vandaagISO())
}

export const isValideDatum = (datum: Date): boolean => {
	return datum.getFullYear().toString().length === 4
}

export const stringDatumEnStringTijdNaarDatumTijd = (datum: string, tijd: string): Date => {
	return new Date(`${datum}T${tijd}`)
}

export const verschilDatumTijdEnNuInMinuten = (datumTijd: string | Date): number => {
	return moment.duration(moment(nuISO()).diff(new Date(datumTijd))).as("minutes")
}

export const numberMinutenNaarStringUrenEnMinuten = (minuten: number): string => {
	return moment.duration(minuten, "minutes").humanize()
}