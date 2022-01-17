import type {Validation} from "./Validation"
import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"

export class TelefoonnummerValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isTelefoonnummerValid(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = validateTelefoonnummer(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}
}

function isTelefoonnummerValid(telefoonnummer: string | undefined): boolean {
	return !telefoonnummer || telefoonnummerCheck(telefoonnummer)
}

function telefoonnummerCheck(telefoonnummer: string | undefined): boolean {
	return telefoonnummer ? isVastNlNummer(telefoonnummer) || isMobielNlNummer(telefoonnummer) || isInformatieNlNummer(telefoonnummer) || isBuitenlandsNummer(telefoonnummer) : true
}

function isVastNlNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(0[0-9]{9})|(0[0-9]{2}( |-)[0-9]{7})|(0[0-9]{3}( |-)[0-9]{6})$/)
}

function isMobielNlNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(06( |-)?[0-9]{8})$/)
}

function isInformatieNlNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(0(8|9)00( |-)?\d{4}(\d{3})?$)$/)
}

function isBuitenlandsNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(\+|00)[0-9 -]{4,15}$/)
}

function exactMatch(telefoonnummer: string, regex: RegExp): boolean {
	const regexResult = regex.exec(telefoonnummer)

	if (regexResult) {
		return regexResult[0].length === telefoonnummer.length
	} else {
		return false
	}
}

function validateTelefoonnummer(telefoonnummer: string | undefined, label: string): ValidationError[] {
	const errors: ValidationError[] = []
	validateTelefoonnummerOngeldigError(telefoonnummer, label, errors)
	return errors
}

function validateTelefoonnummerOngeldigError(telefoonnummer: string | undefined, label: string, errors: ValidationError[]): void {
	if (!isTelefoonnummerValid(telefoonnummer)) {
		errors.push({
			type: "ongeldig",
			label: label,
		})
	}
}