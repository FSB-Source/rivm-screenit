/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */
import {KandidaatAfspraak} from "../../../../../datatypes/mamma/KandidaatAfspraak"
import React from "react"
import {AfspraakZoekFilter} from "../MammaAfspraakMakenPage"
import MammaAfspraakMakenPopup from "./wizard_componenten/MammaAfspraakMakenPopup"
import ProgressiePanel from "../../../../../components/pagina_progressie/ProgressiePanel"
import MammaAfspraakAfsluitendPopup from "./wizard_componenten/MammaAfspraakAfsluitendPopup"
import MammaHerinneringSmsPopup from "./wizard_componenten/MammaHerinneringSmsPopup"
import {AfspraakBevestigingOpties} from "../../../../../datatypes/mamma/AfspraakBevestigingOpties"
import ScreenitBackend from "../../../../../utils/Backend"
import {showToast} from "../../../../../utils/ToastUtil"
import {getString} from "../../../../../utils/TekstPropertyUtil"
import properties from "./wizard_componenten/MammaAfspraakMakenWizardModuleProperties.json"
import huisartsProperties from "../../../gedeeld/huisarts/HuisartsPage.json"
import {ToastMessageType} from "../../../../../datatypes/toast/ToastMessage"
import {AfspraakBevestigingsWizardStap, getAfspraakBevestingWizardStappen} from "../../../../../datatypes/mamma/AfspraakBevestigingsWizardStap"
import MammaBevestigingsOptiePopup from "./wizard_componenten/MammaBevestigingsOptiePopup"
import {useSelector} from "react-redux"
import {State} from "../../../../../datatypes/State"
import {HuisartsBevestigingsPopup, HuisartsBevestigingsPopupType} from "../../../gedeeld/huisarts/HuisartsBevestigingsPopup"
import {Huisarts, MammaGeenHuisartsOptie} from "../../../../../datatypes/Huisarts"
import {bevestigVorige} from "../../../../../api/HuisartsThunkAction"
import {getBvoBaseUrl} from "../../../../../utils/UrlUtil"
import {useThunkDispatch} from "../../../../../index"
import {useSelectedBvo} from "../../../../../utils/Hooks"
import {useNavigate} from "react-router-dom"
import {Bevolkingsonderzoek} from "../../../../../datatypes/Bevolkingsonderzoek"

export type MammaAfspraakBevestigingsWizardProps = {
	setGekozenAfspraak: React.Dispatch<React.SetStateAction<KandidaatAfspraak | undefined>>,
	gekozenAfspraak: KandidaatAfspraak | undefined,
	setAfspraakMakenNietGelukt: React.Dispatch<React.SetStateAction<boolean>>,
	afspraakMakenNietGelukt: boolean,
	zoekAfspraken: (zoekFilter: AfspraakZoekFilter) => void,
	zoekFilter: AfspraakZoekFilter
}

const MammaAfspraakBevestigingsWizard = (props: MammaAfspraakBevestigingsWizardProps) => {
	const dispatch = useThunkDispatch()
	const bvo = useSelectedBvo()
	const navigate = useNavigate()

	const [afspraakBevestiging, setAfspraakBevestiging] = React.useState<AfspraakBevestigingOpties | undefined>(undefined)
	const [wizardStap, setWizardStap] = React.useState<AfspraakBevestigingsWizardStap>(props.afspraakMakenNietGelukt ? AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN_MISLUKT : AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN)

	const huidigeHuisarts = useSelector((state: State) => state.client.mammaDossier.huisartsHuidigeRonde)
	const vorigeHuisarts = useSelector((state: State) => state.client.mammaDossier.huisartsVorigeRonde)
	const vorigeGeenHuisartsOptie: MammaGeenHuisartsOptie | undefined = useSelector((state: State) => state.client.mammaDossier.geenHuisartsOptieVorigeRonde)

	const stappenInWizard: AfspraakBevestigingsWizardStap[] = props.gekozenAfspraak === undefined ? [] : getAfspraakBevestingWizardStappen(props.gekozenAfspraak.toonSmsHerinneringOptie)
	const huisartsVanClientBekend: boolean = !!geefMeestRecenteHuisarts()

	function maakBevestiging(afspraakBevestiging: AfspraakBevestigingOpties) {
		ScreenitBackend.post(`/mamma/afspraak/bevestiging`, afspraakBevestiging)
			.catch((error) => {
				if (error.response.data === "afspraak.bevestiging.niet.mogelijk") {
					showToast(getString(properties.afspraak_maken.toast.geen_bevestiging), getString(properties.afspraak_maken.toast.error.algemeen), ToastMessageType.ERROR)
				}
			})
	}

	function geefMeestRecenteHuisarts(): Huisarts | undefined {
		return huidigeHuisarts ? huidigeHuisarts : vorigeHuisarts
	}

	function bevestigHuisarts() {
		if (geefMeestRecenteHuisarts() === vorigeHuisarts) {
			dispatch(bevestigVorige(vorigeHuisarts, vorigeGeenHuisartsOptie, Bevolkingsonderzoek.MAMMA)).then(() => {
				showToast(getString(huisartsProperties.gedeeld.toasts.opgeslagen.title), getString(huisartsProperties.gedeeld.toasts.opgeslagen.description))
				navigate(getBvoBaseUrl(bvo))
			})
		} else {
			showToast(getString(huisartsProperties.gedeeld.toasts.opgeslagen.title), getString(huisartsProperties.gedeeld.toasts.opgeslagen.description))
			navigate(getBvoBaseUrl(bvo))
		}
	}

	function navigeerNaarHuisartsKiezen() {
		navigate("/mamma/huisarts/zoeken")
	}

	function gaNaarVolgendePopup() {
		setWizardStap(stappenInWizard.at(stappenInWizard.indexOf(wizardStap) + 1)!)
	}

	function gaNaarVorigePopup() {
		setWizardStap(stappenInWizard.at(stappenInWizard.indexOf(wizardStap) - 1)!)
	}

	function bepaalWelkePopupGetoondMoetWorden() {
		switch (wizardStap) {
			case AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN_MISLUKT:
				return toonAfspraakMakenMisluktPopup()
			case AfspraakBevestigingsWizardStap.AFSPRAAK_MAKEN:
				return toonAfspraakMakenPopup()
			case AfspraakBevestigingsWizardStap.AFSPRAAK_BEVESTIGINGSMAIL:
				return toonAfspraakBevestigingsOptieKiezenPopup()
			case AfspraakBevestigingsWizardStap.AFSPRAAK_SMS_HERINNERING:
				return toonSMSHerinneringPopup()
			case AfspraakBevestigingsWizardStap.AFSPRAAK_OVERZICHT:
				return toonAfspraakAfsluitendOverzichtPopup()
			case AfspraakBevestigingsWizardStap.HUISARTS_DOORGEVEN:
				return toonHuisartsDoorgevenPopup()
		}
	}

	function toonAfspraakMakenMisluktPopup() {
		return <MammaAfspraakMakenPopup afspraak={props.gekozenAfspraak!}
										isBevestigingsPopup={false}
										onNext={() => {
											props.setGekozenAfspraak(undefined)
											props.zoekAfspraken(props.zoekFilter)
											props.setAfspraakMakenNietGelukt(false)
											setAfspraakBevestiging(undefined)
										}}/>
	}

	function toonAfspraakMakenPopup() {
		return <MammaAfspraakMakenPopup afspraak={props.gekozenAfspraak!}
										isBevestigingsPopup={true}
										onNext={() => gaNaarVolgendePopup()}
										onFailure={() => {
											props.setAfspraakMakenNietGelukt(true)
											setAfspraakBevestiging(undefined)
										}}
										onAndereAfspraakKiezen={() => {
											props.zoekAfspraken(props.zoekFilter)
											props.setGekozenAfspraak(undefined)
										}}
										setAfspraakBevestiging={setAfspraakBevestiging}
										children={geefProgressiePanel()}/>
	}

	function toonAfspraakBevestigingsOptieKiezenPopup() {
		return <MammaBevestigingsOptiePopup afspraakBevestiging={afspraakBevestiging!}
											onVolgende={() => {
												if (!afspraakBevestiging?.toonSmsOptie) {
													maakBevestiging(afspraakBevestiging!)
												}
												gaNaarVolgendePopup()
											}}
											children={geefProgressiePanel()}
		/>
	}

	function toonSMSHerinneringPopup() {
		return <MammaHerinneringSmsPopup afspraakBevestiging={afspraakBevestiging!}
										 onVolgende={() => {
											 maakBevestiging(afspraakBevestiging!)
											 gaNaarVolgendePopup()
										 }}
										 children={geefProgressiePanel(() => {
											 afspraakBevestiging!.resetKeuzes()
											 gaNaarVorigePopup()
										 })}
		/>
	}

	function toonAfspraakAfsluitendOverzichtPopup() {
		return <MammaAfspraakAfsluitendPopup afspraakBevestiging={afspraakBevestiging!}
											 onHuisartsControleren={() => gaNaarVolgendePopup()}
											 children={geefProgressiePanel()}
		/>
	}

	function toonHuisartsDoorgevenPopup() {
		return <HuisartsBevestigingsPopup type={HuisartsBevestigingsPopupType.DOORGEVEN}
										  huisarts={geefMeestRecenteHuisarts()}
										  onPrimaireKnop={() => huisartsVanClientBekend ? bevestigHuisarts() : navigeerNaarHuisartsKiezen()}
										  onSecundaireKnop={() => huisartsVanClientBekend ? navigeerNaarHuisartsKiezen() : navigate(getBvoBaseUrl(bvo))}
										  onTertiaireKnop={() => navigate(getBvoBaseUrl(bvo))}
		/>
	}

	function geefProgressiePanel(onTerug?: () => void | undefined) {
		return <ProgressiePanel
			aantalPaginas={geefAantalProgressieBollen()}
			huidigePagina={bepaalHuidigPaginaNummer()}
			onTerug={onTerug}/>
	}

	function geefAantalProgressieBollen() {
		return stappenInWizard.length - 1
	}

	function bepaalHuidigPaginaNummer() {
		return stappenInWizard.indexOf(wizardStap) + 1
	}

	return (
		<div>{bepaalWelkePopupGetoondMoetWorden()}</div>
	)
}

export default MammaAfspraakBevestigingsWizard
