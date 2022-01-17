export class Mammograaf {
	id: number
	werkstationIpAdres: string
	aeTitle: string
	screeningsEenheidId: number

	constructor(id: number, werkstationIpAdres: string, aeTitle: string, screeningsEenheidId: number) {
		this.id = id
		this.werkstationIpAdres = werkstationIpAdres
		this.aeTitle = aeTitle
		this.screeningsEenheidId = screeningsEenheidId
	}

}