package nl.rivm.screenit.model.cervix.berichten;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.time.LocalDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.messagequeue.Message;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.Type;

@Setter
@Getter
@Entity
@Table(schema = "cervix", name = "fout_hl7v2_bericht", uniqueConstraints = { @UniqueConstraint(columnNames = "message"), @UniqueConstraint(columnNames = "client") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class CervixFoutHL7v2Bericht extends AbstractHibernateObject
{

	@Column(nullable = false)
	private LocalDateTime responseMoment;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	@Column(nullable = false)
	private String request;

	@Lob
	@Type(type = "org.hibernate.type.TextType")
	private String response;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private Client client;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private BMHKLaboratorium laboratorium;

	@OneToOne(fetch = FetchType.LAZY, optional = false, cascade = javax.persistence.CascadeType.REMOVE)
	@Cascade(CascadeType.DELETE)
	private Message message;

}
