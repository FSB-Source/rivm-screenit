export type MrzData = {
	id_nummer?: string,
	id_type: string;
}

export const parseMRZ = (data: string): MrzData => {
	if (data.length === 30) {
		const id_nummer = data.substr(6, 10)
		return {
			id_nummer: id_nummer,
			id_type: "RIJBEWIJS",
		}
	} else {
		return {
			id_nummer: undefined,
			id_type: "ONBEKEND",
		}
	}
}