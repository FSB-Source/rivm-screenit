import type {FhirUserSession} from "../datatypes/ims/FhirUserSession"
import type {FhirUser} from "../datatypes/ims/FhirUser"
import type {FhirImagingStudy} from "../datatypes/ims/FhirImagingStudy"
import {FhirFocus} from "../datatypes/ims/FhirFocus"

const createUser = (username: string): FhirUser => {
	return {
		identifier: {
			system: "ScreenIT",
			value: username,
			type: {
				coding: {
					code: "Mbb",
					system: "ScreenIT",
				},
			},
		},
	}
}

const createImagingStudy = (uitnodigingsNr: number | string): FhirImagingStudy => {
	return {
		identifier: {
			system: "ScreenIT",
			value: uitnodigingsNr,
		},
		accession: {
			system: "ScreenIT",
			value: uitnodigingsNr,
		},
	}
}

const createFocus = (bsn: string, uitnodigingsNr: number | string): FhirFocus => {
	return {
		patient: {
			identifier: {
				system: "NLMINBIZA",
				value: bsn,
			},
		},
		imagingStudy: createImagingStudy(uitnodigingsNr),
	}
}

export const createStudyIms = (uitnodigingsNr: number, bsn: string, username: string): FhirUserSession => {
	return {
		resourceType: "UserSession",
		user: createUser(username),
		focus: createFocus(bsn, uitnodigingsNr),
		context: {
			worklist: [],
			type: "Worklist",
			value: "UpcomingCases",
		},
	}
}
export const createEmptyStudyIms = (username: string): FhirUserSession => {
	return {
		resourceType: "UserSession",
		user: createUser(username),
		focus: createFocus("", ""),
		context: {
			worklist: [],
			type: "Worklist",
			value: "UpcomingCases",
		},
	}
}
export const createLogonIms = (username: string): FhirUserSession => {
	return {
		resourceType: "UserSession",
		user: createUser(username),
		context: {
			type: "Session",
			value: "LogOn",
		},
	}
}
export const createLogoffIms = (username: string): FhirUserSession => {
	return {
		resourceType: "UserSession",
		user: createUser(username),
		context: {
			type: "Session",
			value: "LogOff",
		},
	}
}