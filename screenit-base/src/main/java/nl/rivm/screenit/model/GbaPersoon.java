package nl.rivm.screenit.model;

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

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.AanduidingBijzonderNederlanderschap;
import nl.rivm.screenit.model.enums.DatumPrecisie;
import nl.rivm.screenit.model.enums.IndicatieGeheim;
import nl.rivm.screenit.model.gba.Land;
import nl.rivm.screenit.model.gba.Nationaliteit;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Persoon;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Setter
@Getter
@Entity
@Table
@Audited
public class GbaPersoon extends Persoon
{
	private static final long serialVersionUID = 1L;

	public static final int MAX_EMAIL_LENGTH = 100;

	public static final int MAX_PHONE_LENGTH = 20;

	@Column(unique = true)
	private String anummer;

	@ManyToOne(cascade = { CascadeType.PERSIST, CascadeType.MERGE, CascadeType.REFRESH, CascadeType.DETACH })
	private BagAdres gbaAdres;

	@OneToOne(cascade = CascadeType.ALL)
	private TijdelijkAdres tijdelijkAdres;

	@OneToOne(cascade = CascadeType.ALL)
	private TijdelijkGbaAdres tijdelijkGbaAdres;

	@ManyToMany
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	@JoinTable(schema = "gedeeld", name = "pat_persoon_gba_nationaliteiten", joinColumns = { @JoinColumn(name = "pat_persoon") })
	private List<Nationaliteit> gbaNationaliteiten = new ArrayList<>();

	@ManyToOne
	@NotAudited
	private Land gbaGeboorteLand;

	@Temporal(TemporalType.DATE)
	private Date datumVertrokkenUitNederland;

	@Enumerated(EnumType.STRING)
	private DatumPrecisie geboortedatumPrecisie = DatumPrecisie.VOLLEDIG;

	private String titelCode;

	@Enumerated(EnumType.STRING)
	private IndicatieGeheim indicatieGeheim;

	@Enumerated(EnumType.STRING)
	private AanduidingBijzonderNederlanderschap aanduidingBijzonderNederlanderschap;

	@Temporal(TemporalType.DATE)
	private Date datumAangaanPartnerschap;

	@Temporal(TemporalType.DATE)
	private Date datumOntbindingPartnerschap;

	private String redenOntbindingPartnerschap;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Gemeente registerGemeenteAkteOverlijden;

	private String akteNummerOverlijden;

	@Temporal(TemporalType.DATE)
	private Date datumAanvangAdreshouding;

	@Temporal(TemporalType.DATE)
	private Date datumVestigingNederland;

	@Enumerated(EnumType.STRING)
	private Aanhef aanhef;

}
