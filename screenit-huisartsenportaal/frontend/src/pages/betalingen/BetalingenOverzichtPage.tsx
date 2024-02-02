/*-
 * ========================LICENSE_START=================================
 * screenit-huisartsenportaal
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
import BasePage from "../BasePage"
import styles from "./BetalingenOverzichtPage.module.scss"
import properties from "./BetalingenOverzichtPage.json"
import {getString} from "../../util/TekstPropertyUtil"
import BaseTabelComponent from "../../components/tabel/BaseTabelComponent"
import {BetalingenOverzichtDto} from "../../state/datatypes/dto/BetalingenOverzichtDto"
import {formatDateTime, parseDate} from "../../util/DateUtil"
import classNames from "classnames"
import {Form, Formik} from "formik"
import {Button, Col, Row} from "react-bootstrap"
import FormSelectField from "../../components/form/select/FormSelectField"
import FormTextField from "../../components/form/text/FormTextField"
import {BetalingenZoekObjectDto} from "../../state/datatypes/dto/BetalingenZoekObjectDto"
import {useAppSelector} from "../../index"
import FormCheckField from "../../components/form/check/FormCheckField"
import {LocatieDto, LocatieStatus} from "../../state/datatypes/dto/LocatieDto"
import LocatieStatusComponent from "../../components/locatie/status/LocatieStatusComponent"
import TabelPagineringComponent from "../../components/tabel/TabelPagineringComponent"
import React from "react"

const BetalingenOverzichtPage = () => {
	const locaties = useAppSelector((state) => state.locaties)?.values.locaties || []
	const resultsPerPage = 10

	return <BasePage title={getString(properties.title)}>
		<div className={styles.style}>
			<BaseTabelComponent<BetalingenOverzichtDto> resultsPerPage={resultsPerPage} url={"/betaling/all"} exportUrl={"/betaling/csv"}
														exportFileName={"overzicht-betalingen.csv"}>
				{(props) => (
					<>
						<Formik<BetalingenZoekObjectDto>
							initialValues={{
								locatie: null,
								alleenZonderBetalingskenmerk: null,
								betalingsdatumTotenMet: null,
								betalingsdatumVanaf: null,
								betalingskenmerk: null,
								clientNaam: null,
							}}
							onSubmit={(zoekObject) => {
								props.setZoekObjectToevoeging({
									property: "betalingenZoekObject",
									value: {
										...zoekObject,
										betalingsdatumTotenMet: parseDate(zoekObject.betalingsdatumTotenMet),
										betalingsdatumVanaf: parseDate(zoekObject.betalingsdatumVanaf),
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
										<FormTextField className={"row my-3"} property={"betalingskenmerk"} label={getString(properties.form.labels.betalingskenmerk)}/>
									</Col>
									<Col md={6}>
										<FormTextField className={"row my-3"} property={"betalingsdatumVanaf"}
													   label={getString(properties.form.labels.betalingsdatumVanaf)} type={"date"}/>
										<FormTextField className={"row my-3"} property={"betalingsdatumTotenMet"}
													   label={getString(properties.form.labels.betalingsdatumTotenMet)} type={"date"}/>
										<FormCheckField className={"row my-3"} property={"alleenZonderBetalingskenmerk"}
														label={getString(properties.form.labels.alleenZonderBetalingskenmerk)}/>
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
									<th onClick={() => props.setSortProperty("bedrag")}>{getString(properties.table.header.bedrag)}</th>
									<th onClick={() => props.setSortProperty("betalingsdatum")}>{getString(properties.table.header.betalingsdatum)}</th>
									<th onClick={() => props.setSortProperty("betalingsKenmerk")}>{getString(properties.table.header.betalingsKenmerk)}</th>
									<th onClick={() => props.setSortProperty("regio")}>{getString(properties.table.header.regio)}</th>
								</tr>
								</thead>
								<tbody>
								{props.results?.betalingen.map(betaling => {
									return <tr>
										<td>{betaling.verrichting?.huisartsLocatieNaam}</td>
										<td>{betaling.verrichting?.clientNaam}</td>
										<td>{betaling.verrichting?.monsterId}</td>
										<td style={{backgroundColor: betaling.debet ? "rgba(250,1,1,0.05)" : "rgba(1,250,1,0.05)"}}>{betaling.bedragString}</td>
										<td>{betaling.betalingsdatum && formatDateTime(new Date(betaling.betalingsdatum))}</td>
										<td>{betaling.betalingsKenmerk}</td>
										<td>{betaling.verrichting?.regio}</td>
									</tr>
								})}
								</tbody>
							</table>
						</div>
						<Row>
							<Col md={6}>
								<span>{getString(properties.table.totaal)} <b>{props.results?.aantalBetalingen || 0}</b>, {getString(properties.table.totaalBedrag)}
									<b>{props.results?.totaalBedrag || "€ 0,00"}</b></span>
							</Col>
							<Col md={6}>
								<Row>
									<TabelPagineringComponent pageCount={Math.ceil(Math.max(1, props.results?.aantalBetalingen || 0) / resultsPerPage)} page={props.page}
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

export default BetalingenOverzichtPage
