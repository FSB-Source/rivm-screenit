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
import {getString} from "../../utils/TekstPropertyUtil"
import classNames from "classnames"
import baseStyles from "../BasePage.module.scss"
import styles from "./BezwaarWijzigenPage.module.scss"
import KruimelpadComponent from "../../components/kruimelpad/KruimelpadComponent"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import React, {useEffect} from "react"
import {Container} from "react-bootstrap"
import {Bezwaar, BezwaarMoment} from "../../datatypes/Bezwaar"
import {getLaatsteBezwaarMoment, saveNieuwBezwaarMoment} from "../../api/BezwaarThunkAction"
import SubmitForm from "../../components/form/SubmitForm"
import {Formik} from "formik"
import {useSelector} from "react-redux"
import {useThunkDispatch} from "../../index"
import {Bevolkingsonderzoek} from "../../datatypes/Bevolkingsonderzoek"
import {setLaatsteBezwaarMomentAction} from "../../actions/BezwaarReduxAction"
import {selectBehoortTotCervixDoelgroep, selectBehoortTotColonDoelgroep, selectBehoortTotMammaDoelgroep} from "../../selectors/BvoSelectors"
import {selectBezwaren} from "../../selectors/BezwaarSelectors"
import BezwaarBlok from "./BezwaarBlok"
import BezwaarInformatieBlok from "./BezwaarInformatieBlok"
import {BezwaarType} from "../../datatypes/BezwaarType"
import {useNavigate} from "react-router-dom"

const BezwaarWijzigenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const properties = require("./BezwaarWijzigenPage.json")
	const behoortTotMammaDoelgroep = useSelector(selectBehoortTotMammaDoelgroep)
	const behoortTotColonDoelgroep = useSelector(selectBehoortTotColonDoelgroep)
	const behoortTotCervixDoelgroep = useSelector(selectBehoortTotCervixDoelgroep)
	const alleBezwaren = useSelector(selectBezwaren)
	const andereZorgverlenersBezwaarTypes: Array<BezwaarType> = [BezwaarType.GEEN_DIGITALE_UITWISSELING_MET_HET_ZIEKENHUIS, BezwaarType.GEEN_SIGNALERING_VERWIJSADVIES]
	let bvoSpecifiekeBezwaren: Bezwaar[] = []
	let zorgverlenerBezwaren: Bezwaar[] = []
	laadBezwaren()

	useEffect(() => {
		dispatch(getLaatsteBezwaarMoment())
		return () => {
			dispatch(setLaatsteBezwaarMomentAction([]))
		}
	}, [dispatch])

	function laadBezwaren() {
		alleBezwaren.filter(bezwaar => (
			(behoortTotMammaDoelgroep && bezwaar.bevolkingsonderzoek === Bevolkingsonderzoek.MAMMA) ||
			(behoortTotCervixDoelgroep && bezwaar.bevolkingsonderzoek === Bevolkingsonderzoek.CERVIX) ||
			(behoortTotColonDoelgroep && bezwaar.bevolkingsonderzoek === Bevolkingsonderzoek.COLON) ||
			bezwaar.bevolkingsonderzoek === null
		)).forEach(bezwaar => {
			if (andereZorgverlenersBezwaarTypes.includes(bezwaar.type)) {
				zorgverlenerBezwaren.push(bezwaar)
			} else {
				bvoSpecifiekeBezwaren.push(bezwaar)
			}
		})
	}

	function getBezwaarString(key: string, subkey: string): string {
		return getString(properties[key][subkey])
	}

	function opslaan(bezwaren: BezwaarMoment) {
		dispatch(saveNieuwBezwaarMoment(bezwaren)).finally(() => navigate("/profiel"))
	}

	return (
		<Container fluid className={classNames(baseStyles.content, baseStyles.slim)}>
			<KruimelpadComponent/>

			<h1>{getString(properties.page.header.title)}</h1>

			<SpanWithHtml className={"mb-3"} value={getString(properties.page.header.description)}/>
			<div className={styles.childrenContainer}>
				<Formik<BezwaarMoment> enableReinitialize={true}
									   initialValues={alleBezwaren}
									   onSubmit={opslaan}>

					{formikProps => (<SubmitForm<BezwaarMoment> title={""}
																formikProps={formikProps}
																buttonLabel={properties.opslaan.button.label}>

						{bvoSpecifiekeBezwaren.length > 0 && bvoSpecifiekeBezwaren.map((b, index) =>
							<BezwaarBlok bezwaar={b} key={b.type + "_" + index} onChange={(event) => {
								formikProps.setFieldValue(String(alleBezwaren.indexOf(b)), event)
							}} abstract={getBezwaarString(b.type, "abstract")}
										 meer={getBezwaarString(b.type, "meer")} standalone={false}
										 titel={getBezwaarString(b.type, "title")}/>,
						)}

						<div className={"mt-5 mb-3"}>
							<div className={"font-weight-bold"}>{getString(properties.overige_zorgverleners.title)}</div>
							<SpanWithHtml value={getString(properties.overige_zorgverleners.text)}/>
						</div>

						{zorgverlenerBezwaren.length > 0 && zorgverlenerBezwaren.map((b, index) =>
							<BezwaarBlok bezwaar={b} key={b.type + "_" + index} onChange={(event) => {
								formikProps.setFieldValue(String(alleBezwaren.indexOf(b)), event)
							}} abstract={getBezwaarString(b.type, "abstract")}
										 meer={getBezwaarString(b.type, "meer")} standalone={false}
										 titel={getBezwaarString(b.type, "title")}/>,
						)}

						{behoortTotCervixDoelgroep && (
							<div className={styles.overigeBezwaren}>
								<label>{getString(properties.uitwisseling_pathologie_databank.title)}</label>
								<BezwaarInformatieBlok abstract={""} meer={getString(properties.uitwisseling_pathologie_databank.meer)} standalone={true}/>
							</div>
						)}

						{behoortTotColonDoelgroep && (
							<div className={styles.overigeBezwaren}>
								<label>{getString(properties.uitwisseling_coloscopiecentrum.title)}</label>
								<BezwaarInformatieBlok abstract={""} meer={getString(properties.uitwisseling_coloscopiecentrum.meer)} standalone={true}/>
							</div>
						)}

						<div className={styles.overigeBezwaren}>
							<label className="font-weight-bold">{getString(properties.overige.gba)}</label>
							<SpanWithHtml value={getString(properties.overige.infolijn)}/>
						</div>
					</SubmitForm>)}
				</Formik>
			</div>
		</Container>
	)
}

export default BezwaarWijzigenPage
