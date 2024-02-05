package nl.rivm.screenit.huisartsenportaal.model;

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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.model.enums.AanvraagStatus;

import org.hibernate.envers.Audited;

@Entity
@Audited
@Getter
@Setter
public class LabformulierAanvraag extends AbstractReferenceObject
{
	@Temporal(TemporalType.TIMESTAMP)
	private Date aanvraagDatum;

	private Integer aantal;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private AanvraagStatus status;

	@Temporal(TemporalType.TIMESTAMP)
	private Date statusDatum;

	@ManyToOne(fetch = FetchType.LAZY)
	private Locatie locatie;

	@ManyToOne(fetch = FetchType.LAZY)
	private Huisarts huisarts;

	private String aangevraagdDoor;

}
