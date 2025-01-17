package nl.rivm.screenit.model;

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

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.OneToOne;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.HuisartsGeslacht;
import nl.topicuszorg.organisatie.model.Adres;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
@Audited
@Getter
@Setter
public abstract class Huisarts extends TablePerClassHibernateObject
{
	private String achternaam;

	private String voorletters;

	private String tussenvoegels;

	@OneToOne(fetch = FetchType.EAGER, cascade = CascadeType.ALL)
	private Adres adres;

	private String praktijknaam;

	private String ediadres;

	private String oorspronkelijkEdiadres;

	private Date gewijzigd;

	private String klantnummer;

	@Enumerated(EnumType.STRING)
	private HuisartsGeslacht geslacht;

	private String telefoonnummer;

	private boolean verwijderd = false;

	private String huisartsAgb;

	private String praktijkAgb;

	private String weergavenaam;
}
