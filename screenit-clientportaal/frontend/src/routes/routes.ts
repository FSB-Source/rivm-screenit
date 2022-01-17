/*-
 * ========================LICENSE_START=================================
 * screenit-clientportaal
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import {RouteComponentProps, RouteProps} from "react-router-dom"
import * as React from "react"
import {ClientContactActieType} from "../datatypes/ClientContactActieType"
import LoginPage from "../pages/authentication/LoginPage"
import DigiDInloggenAfgebrokenPage from "../pages/authentication/DigiDInloggenAfgebrokenPage"
import LogoutPage from "../pages/authentication/LogoutPage"
import NietInBevolkingsonderzoekPage from "../pages/authentication/NietInBevolkingsonderzoekPage"
import {LandingPage} from "../pages/landing/LandingPage"
import {Bevolkingsonderzoek, BevolkingsonderzoekNaam} from "../datatypes/Bevolkingsonderzoek"
import BvoLandingPageController from "../pages/bvo/landing/BvoLandingPageController"
import CervixUitstelAanvragenPage from "../pages/bvo/cervix/CervixUitstelAanvragenPage"
import ZasAanvragenPage from "../pages/bvo/cervix/ZasAanvragenPage"
import CervixHerdrukAanvragenPage from "../pages/bvo/cervix/CervixHerdrukAanvragenPage"
import ColonFitAanvragenPage from "../pages/bvo/colon/ColonFitAanvragenPage"
import ColonAfspraakAfzeggenPage from "../pages/bvo/colon/ColonAfspraakAfzeggenPage"
import ColonAfspraakMakenPage from "../pages/bvo/colon/afspraak/ColonAfspraakMakenPage"
import AfmeldenPage from "../pages/bvo/gedeeld/AfmeldenPage"
import HeraanmeldenPage from "../pages/bvo/gedeeld/HeraanmeldenPage"
import BezwaarPage from "../pages/bvo/BezwaarPage"
import ProfielPage from "../pages/profiel/ProfielPage"
import TelefoonnummerWijzigenPage from "../pages/profiel/TelefoonnummerWijzigenPage"
import TijdelijkAdresWijzigenPage from "../pages/profiel/TijdelijkAdresWijzigenPage"
import MammaAfspraakMakenPage from "../pages/bvo/mamma/afspraak/MammaAfspraakMakenPage"
import HuisartsPage from "../pages/bvo/gedeeld/huisarts/HuisartsPage"
import MammaAfspraakUitstellenPage from "../pages/bvo/mamma/afspraak/MammaAfspraakUitstellenPage"
import {createBrowserHistory} from "history"
import AutoLoginPage from "../pages/authentication/AutoLoginPage"
import {capitalize} from "@material-ui/core"

export type Doelgroep = "mamma" | "cervix" | "colon"

export type RoutePath =
    "/"
    | "/login"
    | "/autologin"
    | "/logout"
    | "/inloggen-geannuleerd"
    | "/niet-in-bevolkingsonderzoek"

    | "/cervix"
    | "/cervix/afmelden"
    | "/cervix/bezwaar"
    | "/cervix/heraanmelden"
    | "/cervix/herdrukken"
    | "/cervix/uitstellen"
    | "/cervix/zas"

    | "/colon"
    | "/colon/afmelden"
    | "/colon/afspraak-maken"
    | "/colon/afspraak-wijzigen"
    | "/colon/afzeggen"
    | "/colon/bezwaar"
    | "/colon/fit"
    | "/colon/heraanmelden"
    | "/colon/huisarts"

    | "/mamma"
    | "/mamma/afspraak"
    | "/mamma/afmelden"
    | "/mamma/bezwaar"
    | "/mamma/heraanmelden"
    | "/mamma/huisarts"
    | "/mamma/uitstellen"

    | "/profiel"
    | "/profiel/adres"
    | "/profiel/telefoonnummer"

export type RouteDef = RouteProps & {
    name: string
    path: RoutePath
    private: boolean
    component:
        | React.ComponentType<RouteComponentProps<any>>
        | React.ComponentType<any>
    requiredContactActions?: ClientContactActieType[]
    doelgroep?: Doelgroep
    redirectPage?: RoutePath
}

export const cpHistory = createBrowserHistory()

export function navigate(pathName: RoutePath) {
    cpHistory.push(pathName)
}

const routes: RouteDef[] = [
    {
        private: false,
        exact: true,
        path: "/login",
        name: "Inloggen",
        component: LoginPage,
    },
    {
        private: false,
        exact: true,
        path: "/autologin",
        name: "Inloggen",
        component: AutoLoginPage,
    },
    {
        private: false,
        exact: true,
        path: "/logout",
        name: "Uitloggen",
        component: LogoutPage,
    },
    {
        private: false,
        exact: true,
        path: "/inloggen-geannuleerd",
        name: "Inloggen geannuleerd",
        component: DigiDInloggenAfgebrokenPage,
    },
    {
        private: false,
        exact: true,
        path: "/niet-in-bevolkingsonderzoek",
        name: "Niet bekend in Mijn Bevolkingsonderzoek",
        component: NietInBevolkingsonderzoekPage,
    },
    {
        private: true,
        exact: true,
        path: "/",
        name: "Hoofdmenu",
        component: LandingPage,
    },
    {
        private: true,
        exact: true,
        path: "/mamma",
        name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.MAMMA]),
        component: BvoLandingPageController,
        doelgroep: "mamma",
        redirectPage: "/",
    },
    {
        private: true,
        exact: true,
        path: "/mamma/huisarts",
        name: "Huisarts",
        component: HuisartsPage,
        doelgroep: "mamma",
        requiredContactActions: [ClientContactActieType.MAMMA_HUISARTS_WIJZIGEN],
        redirectPage: "/mamma",
    },
    {
        private: true,
        exact: true,
        path: "/cervix",
        name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.CERVIX]),
        component: BvoLandingPageController,
        doelgroep: "cervix",
        redirectPage: "/",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/uitstellen",
        name: "Uitstellen deelname",
        component: CervixUitstelAanvragenPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.CERVIX_UITSTEL],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/zas",
        name: "Zelfafnameset aanvragen",
        component: ZasAanvragenPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.CERVIX_ZAS_AANVRAGEN],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/herdrukken",
        name: "Nieuwe uitnodigingsbrief aanvragen",
        component: CervixHerdrukAanvragenPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.CERVIX_HERDRUK],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/colon",
        name: capitalize(BevolkingsonderzoekNaam[Bevolkingsonderzoek.COLON]),
        component: BvoLandingPageController,
        doelgroep: "colon",
        redirectPage: "/",
    },
    {
        private: true,
        exact: true,
        path: "/colon/huisarts",
        name: "Huisarts",
        component: HuisartsPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_HUISARTS_WIJZIGEN],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/colon/fit",
        name: "Nieuwe ontlastingstest aanvragen",
        component: ColonFitAanvragenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_AANVRAGEN_NIEUWE_IFOBT],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/colon/afzeggen",
        name: "Afzeggen afspraak",
        component: ColonAfspraakAfzeggenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/mamma/afmelden",
        name: "Afmelden",
        component: AfmeldenPage,
        requiredContactActions: [ClientContactActieType.MAMMA_AFMELDEN],
        redirectPage: "/mamma",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/afmelden",
        name: "Afmelden",
        component: AfmeldenPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.CERVIX_AFMELDEN],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/colon/afmelden",
        name: "Afmelden",
        component: AfmeldenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_AFMELDEN],
        redirectPage: "/",
    },
    {
        private: true,
        exact: true,
        path: "/mamma/heraanmelden",
        name: "Opnieuw aanmelden",
        component: HeraanmeldenPage,
        doelgroep: "mamma",
        requiredContactActions: [ClientContactActieType.MAMMA_HERAANMELDEN],
        redirectPage: "/mamma",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/heraanmelden",
        name: "Opnieuw aanmelden",
        component: HeraanmeldenPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.CERVIX_HERAANMELDEN],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/colon/heraanmelden",
        name: "Opnieuw aanmelden",
        component: HeraanmeldenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_HERAANMELDEN],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/mamma/bezwaar",
        name: "Bezwaar",
        component: BezwaarPage,
        doelgroep: "mamma",
        requiredContactActions: [ClientContactActieType.BEZWAAR],
        redirectPage: "/mamma",
    },
    {
        private: true,
        exact: true,
        path: "/cervix/bezwaar",
        name: "Bezwaar",
        component: BezwaarPage,
        doelgroep: "cervix",
        requiredContactActions: [ClientContactActieType.BEZWAAR],
        redirectPage: "/cervix",
    },
    {
        private: true,
        exact: true,
        path: "/colon/bezwaar",
        name: "Bezwaar",
        component: BezwaarPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.BEZWAAR],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/profiel",
        name: "Profiel",
        component: ProfielPage,
    },
    {
        private: true,
        exact: true,
        path: "/profiel/telefoonnummer",
        name: "Telefoonnummer",
        component: TelefoonnummerWijzigenPage,
        requiredContactActions: [ClientContactActieType.INZAGE_PERSOONSGEGEVENS],
        redirectPage: "/profiel",
    },
    {
        private: true,
        exact: true,
        path: "/profiel/adres",
        name: "Tijdelijk adres",
        component: TijdelijkAdresWijzigenPage,
        requiredContactActions: [ClientContactActieType.TIJDELIJK_ADRES],
        redirectPage: "/profiel",
    },
    {
        private: true,
        exact: true,
        path: "/mamma/afspraak",
        name: "Afspraak",
        component: MammaAfspraakMakenPage,
        doelgroep: "mamma",
        requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
    },
    {
        private: true,
        exact: true,
        path: "/mamma/uitstellen",
        name: "Onderzoek uitstellen",
        component: MammaAfspraakUitstellenPage,
        doelgroep: "mamma",
        requiredContactActions: [ClientContactActieType.MAMMA_AFSPRAAK_WIJZIGEN, ClientContactActieType.MAMMA_AFSPRAAK_MAKEN],
    },
    {
        private: true,
        exact: true,
        path: "/colon/afspraak-wijzigen",
        name: "Afspraak verzetten",
        component: ColonAfspraakMakenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_AFSPRAAK_WIJZIGEN_AFZEGGEN],
        redirectPage: "/colon",
    },
    {
        private: true,
        exact: true,
        path: "/colon/afspraak-maken",
        name: "Afspraak maken",
        component: ColonAfspraakMakenPage,
        doelgroep: "colon",
        requiredContactActions: [ClientContactActieType.COLON_NIEUWE_AFSPRAAK_AANMAKEN],
        redirectPage: "/colon",
    },
]

export default routes
