
package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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

@Entity
@Table
@Audited
public class GbaPersoon extends Persoon
{

	private static final long serialVersionUID = 1L;

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

	public BagAdres getGbaAdres()
	{
		return gbaAdres;
	}

	public void setGbaAdres(BagAdres gbaAdres)
	{
		this.gbaAdres = gbaAdres;
	}

	public TijdelijkAdres getTijdelijkAdres()
	{
		return tijdelijkAdres;
	}

	public void setTijdelijkAdres(TijdelijkAdres tijdelijkAdres)
	{
		this.tijdelijkAdres = tijdelijkAdres;
	}

	public TijdelijkGbaAdres getTijdelijkGbaAdres()
	{
		return tijdelijkGbaAdres;
	}

	public void setTijdelijkGbaAdres(TijdelijkGbaAdres tijdelijkGbaAdres)
	{
		this.tijdelijkGbaAdres = tijdelijkGbaAdres;
	}

	public Land getGbaGeboorteLand()
	{
		return gbaGeboorteLand;
	}

	public void setGbaGeboorteLand(Land gbaGeboorteLand)
	{
		this.gbaGeboorteLand = gbaGeboorteLand;
	}

	public List<Nationaliteit> getGbaNationaliteiten()
	{
		return gbaNationaliteiten;
	}

	public Date getDatumVertrokkenUitNederland()
	{
		return datumVertrokkenUitNederland;
	}

	public void setDatumVertrokkenUitNederland(Date datumVertrokkenUitNederland)
	{
		this.datumVertrokkenUitNederland = datumVertrokkenUitNederland;
	}

	public DatumPrecisie getGeboortedatumPrecisie()
	{
		return geboortedatumPrecisie;
	}

	public void setGeboortedatumPrecisie(DatumPrecisie geboortedatumPrecisie)
	{
		this.geboortedatumPrecisie = geboortedatumPrecisie;
	}

	public String getTitelCode()
	{
		return titelCode;
	}

	public void setTitelCode(String titelCode)
	{
		this.titelCode = titelCode;
	}

	public IndicatieGeheim getIndicatieGeheim()
	{
		return indicatieGeheim;
	}

	public void setIndicatieGeheim(IndicatieGeheim indicatieGeheim)
	{
		this.indicatieGeheim = indicatieGeheim;
	}

	public Gemeente getRegisterGemeenteAkteOverlijden()
	{
		return registerGemeenteAkteOverlijden;
	}

	public void setRegisterGemeenteAkteOverlijden(Gemeente registerGemeenteAkteOverlijden)
	{
		this.registerGemeenteAkteOverlijden = registerGemeenteAkteOverlijden;
	}

	public String getAkteNummerOverlijden()
	{
		return akteNummerOverlijden;
	}

	public void setAkteNummerOverlijden(String akteNummerOverlijden)
	{
		this.akteNummerOverlijden = akteNummerOverlijden;
	}

	public String getAnummer()
	{
		return anummer;
	}

	public void setAnummer(String anummer)
	{
		this.anummer = anummer;
	}

	public AanduidingBijzonderNederlanderschap getAanduidingBijzonderNederlanderschap()
	{
		return aanduidingBijzonderNederlanderschap;
	}

	public void setAanduidingBijzonderNederlanderschap(AanduidingBijzonderNederlanderschap aanduidingBijzonderNederlanderschap)
	{
		this.aanduidingBijzonderNederlanderschap = aanduidingBijzonderNederlanderschap;
	}

	public Date getDatumAangaanPartnerschap()
	{
		return datumAangaanPartnerschap;
	}

	public void setDatumAangaanPartnerschap(Date datumAangaanPartnerschap)
	{
		this.datumAangaanPartnerschap = datumAangaanPartnerschap;
	}

	public Date getDatumOntbindingPartnerschap()
	{
		return datumOntbindingPartnerschap;
	}

	public void setDatumOntbindingPartnerschap(Date datumOntbindingPartnerschap)
	{
		this.datumOntbindingPartnerschap = datumOntbindingPartnerschap;
	}

	public String getRedenOntbindingPartnerschap()
	{
		return redenOntbindingPartnerschap;
	}

	public void setRedenOntbindingPartnerschap(String redenOntbindingPartnerschap)
	{
		this.redenOntbindingPartnerschap = redenOntbindingPartnerschap;
	}

	public Date getDatumAanvangAdreshouding()
	{
		return datumAanvangAdreshouding;
	}

	public void setDatumAanvangAdreshouding(Date datumAanvangAdreshouding)
	{
		this.datumAanvangAdreshouding = datumAanvangAdreshouding;
	}

	public Date getDatumVestigingNederland()
	{
		return datumVestigingNederland;
	}

	public void setDatumVestigingNederland(Date datumVestigingNederland)
	{
		this.datumVestigingNederland = datumVestigingNederland;
	}

	public void setGbaNationaliteiten(List<Nationaliteit> gbaNationaliteiten)
	{
		this.gbaNationaliteiten = gbaNationaliteiten;
	}

}
