/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import styles from "./VerrichtingenOverzichtPage.module.scss"
import properties from "./VerrichtingenOverzichtPage.json"
import {getString} from "../../util/TekstPropertyUtil"
import BaseTabelComponent from "../../components/tabel/BaseTabelComponent"
import {formatDate, parseDate} from "../../util/DateUtil"
import {VerrichtingenOverzichtDto} from "../../state/datatypes/dto/VerrichtingenOverzichtDto"
import {Form, Formik} from "formik"
import {VerrichtingenZoekObjectDto} from "../../state/datatypes/dto/VerrichtingenZoekObjectDto"
import {Button, Col, Row} from "react-bootstrap"
import {useAppSelector} from "../../index"
import {LocatieDto, LocatieStatus} from "../../state/datatypes/dto/LocatieDto"
import FormSelectField from "../../components/form/select/FormSelectField"
import FormTextField from "../../components/form/text/FormTextField"
import classNames from "classnames"
import LocatieStatusComponent from "../../components/locatie/status/LocatieStatusComponent"
import TabelPagineringComponent from "../../components/tabel/TabelPagineringComponent"
import React from "react"

const VerrichtingenOverzichtPage = () => {
	const locaties = useAppSelector((state) => state.locaties)?.values.locaties || []
	const resultsPerPage = 10

	return <BasePage title={getString(properties.title)}>
		<div className={styles.style}>
			<BaseTabelComponent<VerrichtingenOverzichtDto> resultsPerPage={resultsPerPage} url={"/verrichting/all"} exportUrl={"/verrichting/csv"}
														   exportFileName={"overzicht-verrichtingen.csv"}>
				{(props) => (
					<>
						<Formik<VerrichtingenZoekObjectDto>
							initialValues={{
								clientNaam: "",
								datumUitstrijkje: null,
								locatie: null,
								verrichtingsDatumVanaf: null,
								verrichtingsDatumTotenmet: null,
							}}
							onSubmit={(zoekObject) => {
								props.setZoekObjectToevoeging({
									property: "verrichtingenZoekObject",
									value: {
										...zoekObject,
										datumUitstrijkje: parseDate(zoekObject.datumUitstrijkje),
										verrichtingsDatumVanaf: parseDate(zoekObject.verrichtingsDatumVanaf),
										verrichtingsDatumTotenmet: parseDate(zoekObject.verrichtingsDatumTotenmet),
									},
								})
							}}
						>
							{(formikProps) => <Form className={"mb-5 rounded py-2 px-4"}>
								<Row className={"pt-3"}>
									<Col md={6}>
										<FormSelectField<LocatieDto>
											className={"row my-3"} clearable
											options={locaties.map(locatie => {
												return {value: locatie, label: locatie.naam}
											})} setValue={(v) => formikProps.setFieldValue("locatie", v)} property={"locatie"}
											formatOptionLabel={(value) => {
												return <Row>
													<Col sm={6}>
														<span>{value.label}</span>
													</Col>
													<Col sm={6}>
														{value.value.status !== LocatieStatus.ACTIEF && <LocatieStatusComponent status={value.value.status}/>}
													</Col>
												</Row>
											}}
											label={getString(properties.form.labels.locatie)}/>
										<FormTextField className={"row my-3"} property={"clientNaam"} label={getString(properties.form.labels.clientNaam)}/>
									</Col>
									<Col md={6}>
										<FormTextField className={"row my-3"} property={"datumUitstrijkje"}
													   label={getString(properties.form.labels.datumUitstrijkje)} type={"date"}/>
										<FormTextField className={"row my-3"} property={"verrichtingsDatumVanaf"}
													   label={getString(properties.form.labels.verrichtingsDatumVanaf)} type={"date"}/>
										<FormTextField className={"row my-3"} property={"verrichtingsDatumTotenmet"}
													   label={getString(properties.form.labels.verrichtingsDatumTot)} type={"date"}/>
										<Row>
											<Col md={6}/>
											<Col md={6}>
												<Button type={"submit"} className={"w-100 mb-2"}>
													{getString(properties.form.submit)}
												</Button>
											</Col>
										</Row>
									</Col>
								</Row>
							</Form>}

						</Formik>
						<div className={classNames(styles.tableContainer, "mb-3")}>
							<table>
								<thead>
								<tr>
									<th onClick={() => props.setSortProperty("huisartsLocatie.naam")}>{getString(properties.table.header.huisartsLocatie)}</th>
									<th onClick={() => props.setSortProperty("clientNaam")}>{getString(properties.table.header.clientNaam)}</th>
									<th onClick={() => props.setSortProperty("monsterId")}>{getString(properties.table.header.monsterId)}</th>
									<th onClick={() => props.setSortProperty("datumUitstrijkje")}>{getString(properties.table.header.datumUitstrijkje)}</th>
									<th onClick={() => props.setSortProperty("verrichtingsDatum")}>{getString(properties.table.header.verrichtingsDatum)}</th>
									<th onClick={() => props.setSortProperty("formulierOntvangstDatum")}>{getString(properties.table.header.formulierOntvangstDatum)}</th>
									<th onClick={() => props.setSortProperty("regio")}>{getString(properties.table.header.regio)}</th>
								</tr>
								</thead>

								<tbody>
								{props.results?.verrichtingen.map(verrichting => {
									return <tr key={verrichting.huisartsportaalId}>
										<td>{verrichting.huisartsLocatie?.naam}</td>
										<td>{verrichting.clientNaam}</td>
										<td>{verrichting.monsterId}</td>
										<td>{verrichting.datumUitstrijkje && formatDate(new Date(verrichting.datumUitstrijkje))}</td>
										<td>{verrichting.verrichtingsDatum && formatDate(new Date(verrichting.verrichtingsDatum))}</td>
										<td>{verrichting.formulierOntvangstDatum && formatDate(new Date(verrichting.formulierOntvangstDatum))}</td>
										<td>{verrichting.regio}</td>
									</tr>
								})}
								</tbody>
							</table>
						</div>
						<Row>
							<Col md={6}>
								<span>{getString(properties.table.totaal)} <b>{props.results?.aantalVerrichtingen || 0}</b></span>
							</Col>
							<Col md={6}>
								<Row>
									<TabelPagineringComponent pageCount={Math.ceil(Math.max(1, props.results?.aantalVerrichtingen || 0) / resultsPerPage)} page={props.page}
															  setPage={props.setPage}/>
								</Row>
								<Row>
									<Col md={7}/>
									<Col md={5}>
										<Button variant={"secondary"} className={"w-100"} onClick={props.export}>
											{getString(properties.table.export)}
										</Button>
									</Col>
								</Row>
							</Col>
						</Row>
					</>
				)}
			</BaseTabelComponent>
		</div>
	</BasePage>
}

export default VerrichtingenOverzichtPage
