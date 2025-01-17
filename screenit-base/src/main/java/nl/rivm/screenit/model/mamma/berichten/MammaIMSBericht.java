package nl.rivm.screenit.model.mamma.berichten;

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
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Lob;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.berichten.enums.BerichtStatus;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.mamma.enums.MammaHL7v24ORMBerichtStatus;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Type;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "mamma", name = "ims_bericht")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public class MammaIMSBericht extends AbstractHibernateObject
{

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date ontvangstDatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private BerichtStatus berichtStatus;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	@Column(nullable = false)
	@NotAudited
	private String hl7Bericht;

	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private MammaHL7v24ORMBerichtStatus ormStatus;

	@Column(nullable = false)
	private String bsn;

	@Column(nullable = false)
	private long accessionNumber;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekType onderzoekType;

	@Column(nullable = false, unique = true)
	private String messageId;
}
