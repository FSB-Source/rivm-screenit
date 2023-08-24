package nl.rivm.screenit.model.cervix;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.huisartsenportaal.ICervixHuisartsLocatie;
import nl.rivm.screenit.huisartsenportaal.enums.CervixLocatieStatus;
import nl.rivm.screenit.model.IActief;
import nl.rivm.screenit.model.cervix.enums.CervixHuisartsLocatieMutatieSoort;
import nl.rivm.screenit.model.cervix.facturatie.CervixVerrichting;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;

@Entity
@Table(
	schema = "cervix",
	name = "huisarts_locatie",
	uniqueConstraints = { @UniqueConstraint(columnNames = "locatie_adres") },
	indexes = {
		@Index(name = "idx_cervix_huisarts_locatie_naam", columnList = "naam") })
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "organisatie.cache")
@Audited
@Setter
@Getter
public class CervixHuisartsLocatie extends AbstractHibernateObject implements ICervixHuisartsportaalObject, IActief, ICervixHuisartsLocatie
{
	@OneToOne(fetch = FetchType.LAZY, optional = false)
	private CervixHuisartsAdres locatieAdres;

	@ManyToOne(fetch = FetchType.EAGER, optional = false)
	private CervixHuisarts huisarts;

	@OneToMany(mappedBy = "huisartsLocatie", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixHuisartsBericht> huisartsberichten = new ArrayList<>();

	@OneToMany(mappedBy = "huisartsLocatie", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixLabformulier> labformulieren = new ArrayList<>();

	@OneToMany(mappedBy = "huisartsLocatie", fetch = FetchType.LAZY)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixVerrichting> verrichtingen;

	@Column
	private Long huisartsportaalId;

	@Column(length = 200, nullable = false)
	private String naam;

	@Column(length = 34, nullable = false)
	private String iban;

	@Column(length = 70, nullable = false)
	private String ibanTenaamstelling;

	@Column(nullable = false)
	private String zorgmailklantnummer;

	@Enumerated(EnumType.STRING)
	private CervixHuisartsLocatieMutatieSoort mutatieSoort;

	@Temporal(TemporalType.TIMESTAMP)
	private Date mutatiedatum;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private CervixLocatieStatus status;

	@Column(length = 255, nullable = true, unique = false)
	private String verificatieCode;

	@Column(nullable = true)
	private Boolean moetVerifierenVoorActivatie = false;

	@Override
	public void setScreenitId(Long id)
	{
		if (id != null)
		{
			setId(id);
		}
	}

	@Override
	public Long getScreenitId()
	{
		return getId();
	}

	@Override
	@Transient
	public Boolean getActief()
	{
		return !CervixLocatieStatus.INACTIEF.equals(getStatus());
	}

	@Override
	@Transient
	public void setActief(Boolean actief)
	{

	}
}
