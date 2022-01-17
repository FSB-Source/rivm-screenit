import moment from "moment"

export const vandaagPlusDagen = (aantal: number): string => {
	return moment().add(aantal, "d").format("YYYY-MM-DDTHH:mm:ss")
}