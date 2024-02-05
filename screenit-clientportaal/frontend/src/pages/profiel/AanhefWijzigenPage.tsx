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
import React from "react"
import ActieBasePage from "../ActieBasePage"
import * as Yup from "yup"
import {useThunkDispatch} from "../../index"
import {getString} from "../../utils/TekstPropertyUtil"
import {useSelector} from "react-redux"
import {State} from "../../datatypes/State"
import properties from "./AanhefWijzigenPage.json"
import {Formik} from "formik"
import SubmitForm from "../../components/form/SubmitForm"
import {useNavigate} from "react-router-dom"
import {showToast} from "../../utils/ToastUtil"
import {saveAanhef} from "../../api/AanspreekvormWijzigenThunkAction"
import {FormControl, FormControlLabel, Radio, RadioGroup} from "@mui/material"
import {AanhefType} from "../../datatypes/aanhef/AanhefType"
import {Persoon} from "../../datatypes/Persoon"
import {Geslacht} from "../../datatypes/Geslacht"

const AanhefWijzigenPage = () => {
	const dispatch = useThunkDispatch()
	const navigate = useNavigate()
	const persoon = useSelector((state: State) => state.client.persoon)

	const initialValues = {
		aanhef: gekozenAanhef(persoon),
	}

	const validatieSchema: Yup.AnyObjectSchema = Yup.object().shape({
		aanhef: Yup.string().required(getString(properties.form.error)),
	})

	return (
        <ActieBasePage
			bvoName={""}
			title={getString(properties.page.title)}
			description={getString(properties.page.description)}>

			<Formik initialValues={initialValues}
					validationSchema={validatieSchema}
					onSubmit={(aanhef) => {
						dispatch(saveAanhef(aanhef)).then(() => {
							showToast(getString(properties.toast.title), getString(properties.toast.description))
							navigate("/profiel")
						})
					}}>

				{formikProps => (
					<SubmitForm title={getString(properties.form.title)}
								formikProps={formikProps}
								buttonLabel={getString(properties.form.button)}>

						<FormControl variant="standard" required component="fieldset">
							<RadioGroup
								name="aanhef"
								onChange={formikProps.handleChange}
								value={formikProps.values.aanhef || ""}>
								<ul>
									<li>
										<FormControlLabel
											value={AanhefType.GEACHTE}
											data-testid={"radio_geachte"}
											control={<Radio/>}
											label={getString(properties.form.radiobutton.geachte, [persoon.voorletters, persoon.aanspreekTussenvoegselEnAchternaam])}/>
									</li>
									<li><FormControlLabel
										value={AanhefType.GEACHTE_HEER}
										data-testid={"radio_geachte_heer"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.geachte_heer, [persoon.aanspreekTussenvoegselEnAchternaam])}/>
									</li>
									<li><FormControlLabel
										value={AanhefType.GEACHTE_MEVROUW}
										data-testid={"radio_geachte_mevrouw"}
										control={<Radio/>}
										label={getString(properties.form.radiobutton.geachte_mevrouw, [persoon.aanspreekTussenvoegselEnAchternaam])}/>
									</li>
								</ul>
							</RadioGroup>
						</FormControl>
					</SubmitForm>)}
			</Formik>
		</ActieBasePage>
    );

	function gekozenAanhef(persoon: Persoon) {
		let aanhef = persoon.aanhef
		if (persoon.aanhef === null) {
			if (persoon.geslacht === Geslacht.VROUW) {
				aanhef = AanhefType.GEACHTE_MEVROUW
			} else if (persoon.geslacht === Geslacht.MAN) {
				aanhef = AanhefType.GEACHTE_HEER
			} else {
				aanhef = AanhefType.GEACHTE
			}
		}
		return aanhef
	}
}

export default AanhefWijzigenPage
