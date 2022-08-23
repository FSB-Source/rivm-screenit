/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import BasePage from "../BasePage"
import styles from "./LabformulierenAanvragenPage.module.scss"
import properties from "./LabformulierenAanvragenPage.json"
import validatieProperties from "../../util/ValidatieUtil.json"
import {getString} from "../../util/TekstPropertyUtil"
import BaseTabelComponent from "../../components/tabel/BaseTabelComponent"
import {LabformulierAanvragenOverzichtDto} from "../../state/datatypes/dto/LabformulierAanvragenOverzichtDto"
import {getAanvraagStatusText, NieuweAanvraagDto} from "../../state/datatypes/dto/AanvraagDto"
import {useAppSelector, useAppThunkDispatch} from "../../index"
import {LocatieDto, LocatieStatus} from "../../state/datatypes/dto/LocatieDto"
import {Form, Formik} from "formik"
import {Button, Col, Row} from "react-bootstrap"
import FormTextField from "../../components/form/text/FormTextField"
import FormSelectField from "../../components/form/select/FormSelectField"
import classNames from "classnames"
import * as Yup from "yup"
import {loadingThunkAction} from "../../api/LoadingThunkAction"
import {validatingRequest} from "../../util/Backend"
import {createActionPushToast} from "../../state/ToastsState"
import {ToastType} from "../../state/datatypes/Toast"
import {formatDateTime} from "../../util/DateUtil"

const LabformulierenAanvragenPage = () => {
	const dispatch = useAppThunkDispatch()
	const huisarts = useAppSelector((state) => state.huisarts)!
	const locaties = useAppSelector((state) => state.huisarts)?.locaties.filter(locatie => locatie.status === LocatieStatus.ACTIEF) || []

	return <BasePage title={getString(properties.title)} description={getString(properties.description)}>
		<div className={styles.style}>
			<BaseTabelComponent<LabformulierAanvragenOverzichtDto> resultsPerPage={10} url={"/aanvragen/huisarts"}>
				{(props) => (
					<>
						<div className={classNames(styles.tableContainer, "mb-3")}>
							<table>
								<thead>
								<tr>
									<th onClick={() => props.setSortProperty("aanvraagDatum")}>{getString(properties.table.header.aangevraagdOp)}</th>
									<th onClick={() => props.setSortProperty("aantal")}>{getString(properties.table.header.aantal)}</th>
									<th onClick={() => props.setSortProperty("status")}>{getString(properties.table.header.status)}</th>
									<th onClick={() => props.setSortProperty("statusDatum")}>{getString(properties.table.header.statusDatum)}</th>
									<th onClick={() => props.setSortProperty("aangevraagdDoor")}>{getString(properties.table.header.aangevraagdDoor)}</th>
									<th onClick={() => props.setSortProperty("locatie.naam")}>{getString(properties.table.header.locatie)}</th>
								</tr>
								</thead>
								<tbody>
								{props.results?.aanvragen.map(aanvraag => {
									return <tr key={aanvraag.huisartsportaalId}>
										<td>{aanvraag.aanvraagDatum && formatDateTime(new Date(aanvraag.aanvraagDatum))}</td>
										<td>{aanvraag.aantal}</td>
										<td>{aanvraag.status && getAanvraagStatusText[aanvraag.status]}</td>
										<td>{aanvraag.statusDatum && formatDateTime(new Date(aanvraag.statusDatum))}</td>
										<td>{aanvraag.aangevraagdDoor}</td>
										<td>{aanvraag.locatie.naam}</td>
									</tr>
								})}
								</tbody>
							</table>
						</div>
						<span>{getString(properties.table.totaal)} <b>{props.results?.aantalAanvragen || 0}</b></span>
						<Formik<NieuweAanvraagDto>
							initialValues={{
								aantal: 0,
								locatie: null,
							}}
							validationSchema={Yup.object({
								aantal: Yup.number()
									.min(10, getString(validatieProperties.min, [getString(properties.form.labels.aantal)]))
									.required(getString(validatieProperties.required)),
								locatie: Yup.mixed().required(getString(validatieProperties.required)),
							})}
							onSubmit={(values) => {
								dispatch(loadingThunkAction(validatingRequest("/aanvragen/huisarts/" + huisarts.huisartsportaalId, "POST", values))).then(() => {
									dispatch(createActionPushToast({type: ToastType.SUCCESS, message: getString(properties.form.toast.success)}))
									props.refresh()
								})
							}}
						>
							{(formikProps) => <Form className={classNames(styles.aanvraagForm, "rounded", "mt-4", "pb-3")}>
								<Row className={"pt-3"}>
									<h4>{getString(properties.form.title)}</h4>
								</Row>
								<hr/>
								<Row className={"px-4"}>
									<Col md={6}>
										<FormTextField className={"row my-3"} property={"aantal"} label={getString(properties.form.labels.aantal)} required type={"number"}
													   error={formikProps.errors.aantal}/>
									</Col>
									<Col md={6}>
										<FormSelectField<LocatieDto>
											className={"row my-3"} clearable required error={formikProps.errors.locatie}
											options={locaties.map(locatie => {
												return {value: locatie, label: locatie.naam}
											})} setValue={(v) => formikProps.setFieldValue("locatie", v)} property={"locatie"}
											label={getString(properties.form.labels.locatie)}/>
									</Col>
								</Row>
								<Row className={"pt-2 px-4"}>
									<Col md={9}/>
									<Col md={3}>
										<Button type={"submit"}>
											{getString(properties.form.submit)}
										</Button>
									</Col>
								</Row>
							</Form>}
						</Formik>
					</>
				)}
			</BaseTabelComponent>
		</div>
	</BasePage>
}

export default LabformulierenAanvragenPage
