import {RootState} from "../Store"

export const createStateHelper = (stateSlice: Partial<RootState> = {}): RootState => {
	return {
		activeStudyForIms: null,
		afsprakenById: new Map(),
		autorisatie: {inschrijven: false, connectiestatus: false, kwaliteitsopname: false, onderzoeken: false, signaleren: false},
		bezigMetKwaliteitsopnameVolgnr: 0,
		clientenById: new Map(),
		connectieStatus: {
			imsConnectieStatus: "OK",
			imsConnectieStatusTimestamp: "",
			mammograafConnectieStatusByAeTitle: new Map(),
		},
		daglijstDatum: "01-01-2023",
		dagverslag: new Map(),
		dubbeleInstantie: false,
		environmentInfo: {
			dagenDaglijstOphalenLimiet: 0,
			environment: "Test",
			huidigWerkstationIpAdres: "",
			nfcEnabled: true, timestamp: "",
			tomosyntheseMogelijk: true,
			version: "23.4",
		},
		error: {errorReferentie: ""},
		formsByFormId: new Map(),
		heeftWijzigingen: false,
		huidigeMammograafId: 0,
		huisartsenById: new Map(),
		loginStatus: {inlogActief: false, stopIdentificerenTotYubikeyEraf: false},
		mammografenById: new Map(),
		mammografenStatus: [],
		navigation: {
			afspraakId: 0,
			clientId: 0,
			subPagina: undefined,
			tab: "Daglijst",
		},
		nietAfgeslotenVanaf: null,
		onderzoekByAfspraakId: new Map(),
		online: false,
		opgehaaldeDagen: new Set<string>(),
		pendingUpdates: null,
		planning: new Map(),
		popup: {
			akkoordString: "",
			alleenOnline: false,
			annulerenString: "",
			body: undefined,
			callback(args: unknown): void {
			},
			cancelCallback(args: unknown): void {
			},
			titel: "", visible: false,
		},
		seGebruikers: new Map(),
		session: null,
		signaleringByAfspraakId: new Map(),
		visueleInspectieAfbeeldingByAfspraakId: new Map(),
		zorginstellingen: new Map(),
		...stateSlice,
	}
}