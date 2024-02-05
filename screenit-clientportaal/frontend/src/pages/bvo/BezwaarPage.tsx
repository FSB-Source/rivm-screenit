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
import React, {useEffect} from "react"
import styles from "./BezwaarPage.module.scss"
import {useSelectedBvo} from "../../utils/Hooks"
import {BevolkingsonderzoekNaam} from "../../datatypes/Bevolkingsonderzoek"
import {useSelector} from "react-redux"
import ActieBasePage from "../ActieBasePage"
import {getLaatsteBezwaarMoment, saveNieuwBezwaarMoment} from "../../api/BezwaarThunkAction"
import {State} from "../../datatypes/State"
import {useThunkDispatch} from "../../index"
import {getString} from "../../utils/TekstPropertyUtil"
import BezwaarInformatieBlok from "./BezwaarInformatieBlok"
import BezwaarOverigeMeerInformatieBlokken from "./BezwaarOverigeMeerInformatieBlokken"
import {getOverigeBezwaren} from "../../datatypes/BezwaarOverigeType"
import {BezwaarMoment} from "../../datatypes/Bezwaar"
import {setLaatsteBezwaarMomentAction} from "../../actions/BezwaarReduxAction"
import SpanWithHtml from "../../components/span/SpanWithHtml"
import {Formik} from "formik"
import SubmitForm from "../../components/form/SubmitForm"
import {Checkbox} from "@mui/material"
import {useNavigate} from "react-router-dom"
import {getBvoBaseUrl} from "../../utils/UrlUtil"

const BezwaarPage = () => {
	const dispatch = useThunkDispatch()
	const selectedBvo = useSelectedBvo()!
	const navigate = useNavigate()
	const bezwaren = useSelector((state: State) => state.client.laatsteBezwaarMoment)
	const OVERIGE = "OVERIGE"
	const properties = require("./BezwaarType.json")

	useEffect(() => {
		selectedBvo && dispatch(getLaatsteBezwaarMoment(selectedBvo))

		return () => {
			dispatch(setLaatsteBezwaarMomentAction([]))
		}
	}, [selectedBvo, dispatch])

	function getBezwaarString(key: string, subkey: string) {
		return getString(properties[key][subkey][selectedBvo]) || getString(properties[key][subkey]["default"])
	}

	const getInformatieBlok = (bezwaarType: string): JSX.Element => {
		const abstract = getBezwaarString(bezwaarType, "abstract")
		const meer = getBezwaarString(bezwaarType, "meer")
		return <BezwaarInformatieBlok abstract={abstract}>
			{(meer || bezwaarType === OVERIGE) &&
				<>
					<SpanWithHtml className={styles.meerTekst} value={meer}/>
					{(bezwaarType === OVERIGE) &&
						<BezwaarOverigeMeerInformatieBlokken types={getOverigeBezwaren[selectedBvo]}/>}
				</>}
		</BezwaarInformatieBlok>
	}

	return (
		<ActieBasePage
			bvoName={BevolkingsonderzoekNaam[selectedBvo]}
			title={getString(properties.page.header.title)}
			description={properties.page.header.description}
			hintBegin={properties.page.header.hint}>

			<Formik<BezwaarMoment> enableReinitialize={true}
								   initialValues={bezwaren}
								   onSubmit={(bezwaren: BezwaarMoment) => {
									   dispatch(saveNieuwBezwaarMoment(selectedBvo, bezwaren)).finally(() => navigate(getBvoBaseUrl(selectedBvo)))
								   }}>

				{formikProps => (<SubmitForm<BezwaarMoment> title={""}
															formikProps={formikProps}
															buttonLabel={properties.opslaan.button.label}>

					{bezwaren.map(b => (
						<div key={b.type} className={styles.bezwaarBlok}>
							<div className={styles.checkBox}>
								<Checkbox name={b.type}
										  defaultChecked={b.active}
										  onChange={(event) => {
											  formikProps.setFieldValue(String(bezwaren.indexOf(b)), {type: event.target.name, active: event.target.checked})
										  }}/>
								<label className={styles.label} htmlFor={b.type}>{getBezwaarString(b.type, "title")}</label>
							</div>
							{getInformatieBlok(b.type)}
						</div>
					))}

					<div className={styles.bezwaarBlok}>
						<div className={styles.informatie}>
							<label className={styles.overigTitle}>{getBezwaarString(OVERIGE, "title")}</label>
						</div>
						{getInformatieBlok(OVERIGE)}
					</div>

				</SubmitForm>)}

			</Formik>

		</ActieBasePage>
	)
}

export default BezwaarPage
