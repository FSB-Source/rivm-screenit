package nl.rivm.screenit.model.mamma;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
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
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.enums.MammaOnderzoekType;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.mamma.enums.ExtraFotosReden;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekRedenFotobespreking;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.OnderbrokenOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.OnvolledigOnderzoekOption;
import nl.rivm.screenit.model.mamma.enums.SuboptimaleInsteltechniek;
import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.FetchMode;
import org.hibernate.annotations.FetchProfile;
import org.hibernate.envers.Audited;

@Entity
@Getter
@Setter
@Table(
	schema = "mamma",
	name = "onderzoek",
	uniqueConstraints = { @UniqueConstraint(columnNames = "mammografie"), @UniqueConstraint(columnNames = "signaleren"), @UniqueConstraint(columnNames = "laatste_beoordeling") },
	indexes = {
		@Index(name = "idx_mamma_onderzoek_status", columnList = "status"),
	})
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "mamma.cache")
@Audited
@FetchProfile(
	name = "kansberekening",
	fetchOverrides = {
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "laatsteBeoordeling", mode = FetchMode.JOIN),
		@FetchProfile.FetchOverride(entity = MammaOnderzoek.class, association = "mammografie", mode = FetchMode.JOIN),
	})
public class MammaOnderzoek extends AbstractHibernateObject
{
	@OneToOne(optional = false, fetch = FetchType.LAZY, mappedBy = "onderzoek")
	private MammaAfspraak afspraak;

	@Column(nullable = false)
	@Temporal(TemporalType.TIMESTAMP)
	private Date creatieDatum;

	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	private MammaScreeningsEenheid screeningsEenheid;

	@OneToMany(mappedBy = "onderzoek", fetch = FetchType.LAZY, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE,
		javax.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private List<MammaBeoordeling> beoordelingen = new ArrayList<>();

	@OneToOne(fetch = FetchType.LAZY, cascade = javax.persistence.CascadeType.ALL)
	private MammaBeoordeling laatsteBeoordeling;

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE,
		javax.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaMammografie mammografie;

	@OneToOne(fetch = FetchType.LAZY, optional = true, cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE,
		javax.persistence.CascadeType.REMOVE })
	@Cascade({ CascadeType.DELETE, CascadeType.SAVE_UPDATE })
	private MammaSignaleren signaleren;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekStatus status;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	private ZorgInstelling eerderMammogramZorginstelling;

	@Column
	private Integer eerderMammogramJaartal;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private SuboptimaleInsteltechniek suboptimaleInsteltechniek;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekRedenFotobespreking redenFotobespreking;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String opmerkingMbber;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String opmerkingVoorRadioloog;

	@Column(nullable = false)
	private Boolean operatieRechts;

	@Column(nullable = false)
	private Boolean operatieLinks;

	@Column
	@Enumerated(EnumType.STRING)
	private MammaAmputatie amputatie;

	@Column
	private String aanvullendeInformatieOperatie;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private OnvolledigOnderzoekOption onvolledigOnderzoek;

	@Column(nullable = true)
	@Enumerated(EnumType.STRING)
	private OnderbrokenOnderzoekOption onderbrokenOnderzoek;

	@ElementCollection(targetClass = ExtraFotosReden.class)
	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	@CollectionTable(schema = "mamma", name = "extra_fotos_reden")
	private List<ExtraFotosReden> extraFotosRedenen = new ArrayList<>();

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String adviesHuisarts;

	@Column(nullable = true)
	@Temporal(TemporalType.TIMESTAMP)
	private Date afgerondOp;

	@ManyToOne(fetch = FetchType.LAZY)
	private InstellingGebruiker extraMedewerker;

	@Column(nullable = false)
	private boolean isDoorgevoerd;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MammaOnderzoekType onderzoekType;

	@OneToOne(fetch = FetchType.LAZY, mappedBy = "onderzoek")
	private MammaAdhocMeekijkverzoek meekijkverzoek;
}
