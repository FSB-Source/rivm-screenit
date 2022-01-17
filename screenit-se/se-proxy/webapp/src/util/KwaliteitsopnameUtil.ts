export const parseKwaliteitsopnameVolgNr = (accessionNumber: string): number => {
	return parseInt(accessionNumber.slice(-2), 10)
}