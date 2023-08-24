import type {Validation} from "./Validation"
import type {ValidationError} from "../datatypes/Error"
import {getErrorMessage} from "../datatypes/Error"

export class TelefoonnummerValidator<T extends string | undefined> implements Validation<T> {
	isValid(value: string | undefined): boolean {
		return isTelefoonnummerValideOfLeeg(value)
	}

	getErrorMessage(value: string | undefined, fieldLabel: string): string {
		const errors: ValidationError[] = this.validateTelefoonnummerOngeldigError(value, fieldLabel)
		if (errors.length > 0) {
			return getErrorMessage(errors[0])
		}
		return ""
	}

	validateTelefoonnummerOngeldigError(telefoonnummer: string | undefined, label: string): ValidationError[] {
		const errors: ValidationError[] = []
		if (!this.isValid(telefoonnummer)) {
			errors.push({
				type: "ongeldig",
				label: label,
			})
		}
		return errors
	}
}

export class MobielnummerValidator<T extends string | undefined> extends TelefoonnummerValidator<T> {
	isValid(value: string | undefined): boolean {
		return isMobielnummerValideOfLeeg(value)
	}
}

function isTelefoonnummerValideOfLeeg(telefoonnummer: string | undefined): boolean {
	return !telefoonnummer || telefoonnummerCheck(telefoonnummer)
}

function telefoonnummerCheck(telefoonnummer: string): boolean {
	return telefoonnummer ? isVastNlNummer(telefoonnummer) || isMobielNlNummer(telefoonnummer) || isInformatieNlNummer(telefoonnummer) || isBuitenlandsNummer(telefoonnummer) : true
}

function isMobielnummerValideOfLeeg(mobielnummer: string | undefined): boolean {
	return !mobielnummer || isMobielNlNummer(mobielnummer)
}

function isVastNlNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(0[0-9]{9})|(0[0-9]{2}( |-)[0-9]{7})|(0[0-9]{3}( |-)[0-9]{6})$/)
}

function isMobielNlNummer(telefoonnummer: string): boolean {
	return exactMatch(telefoonnummer, /^(06|\+316|00316)[- ]?\d{8}$/)
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
