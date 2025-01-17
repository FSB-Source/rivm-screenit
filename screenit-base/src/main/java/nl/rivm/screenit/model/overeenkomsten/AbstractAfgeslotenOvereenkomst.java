package nl.rivm.screenit.model.overeenkomsten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.DiscriminatorColumn;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.SingleTableHibernateObject;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Getter
@Setter
@Table(schema = "gedeeld", indexes = { @Index(name = "IDX_AFG_OVEREENKOMST_AKKOORD", columnList = "akkoordDatum, eindDatum, startDatum") })
@DiscriminatorColumn(length = HibernateMagicNumber.L100)
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
public abstract class AbstractAfgeslotenOvereenkomst extends SingleTableHibernateObject
{
	@Column(unique = true)
	private String code;

	@Temporal(TemporalType.DATE)
	private Date startDatum;

	@Temporal(TemporalType.DATE)
	private Date eindDatum;

	@Temporal(TemporalType.DATE)
	private Date akkoordDatum;

	private boolean nieuwereOvereenkomst = false;

	@ManyToOne(optional = false)
	private ScreeningOrganisatie screeningOrganisatie;

	@ManyToOne
	private Overeenkomst overeenkomst;

	private int volgnummer;

	@ManyToOne
	private UploadDocument gescandDocument;

	private boolean teAccoderen;

}
