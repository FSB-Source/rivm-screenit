package nl.rivm.screenit.model.verwerkingverslag.cervix;

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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import nl.topicuszorg.hibernate.object.model.AbstractHibernateObject;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;

@Entity
@Table(schema = "cervix", name = "gevolgen_labproces_verwerken_rapportage")
@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
public class CervixGevolgenLabprocesVerwerkenRapportage extends AbstractHibernateObject
{
	@OneToMany(mappedBy = "rapportage", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixGevolgenLabprocesVerwerkenRapportageBriefType> briefTypen = new ArrayList<>();

	@OneToMany(mappedBy = "rapportage", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType> huisartsberichtTypen = new ArrayList<>();

	@Temporal(TemporalType.TIMESTAMP)
	@Column(nullable = false)
	private Date datumVerwerking;

	private long aantalInLabproces;

	private long aantalDefinitiefHeraangemeld;

	private long aantalEenmaligHeraangemeld;

	private long aantalUitnodigingenUitstrijkje;

	private long aantalUitnodigingenZas;

	private long aantalInVervolgonderzoek;

	private long aantalRondenGesloten;

	private long totaalAantalBrieven;

	private long totaalAantalHuisartsberichten;

	public List<CervixGevolgenLabprocesVerwerkenRapportageBriefType> getBriefTypen()
	{
		return briefTypen;
	}

	public void setBriefTypen(List<CervixGevolgenLabprocesVerwerkenRapportageBriefType> briefTypen)
	{
		this.briefTypen = briefTypen;
	}

	public List<CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType> getHuisartsberichtTypen()
	{
		return huisartsberichtTypen;
	}

	public void setHuisartsberichtTypen(List<CervixGevolgenLabprocesVerwerkenRapportageHuisartsberichtType> huisartsberichten)
	{
		this.huisartsberichtTypen = huisartsberichten;
	}

	public Date getDatumVerwerking()
	{
		return datumVerwerking;
	}

	public void setDatumVerwerking(Date datumVerwerking)
	{
		this.datumVerwerking = datumVerwerking;
	}

	public long getAantalInLabproces()
	{
		return aantalInLabproces;
	}

	public void setAantalInLabproces(long aantalInLabproces)
	{
		this.aantalInLabproces = aantalInLabproces;
	}

	public long getAantalDefinitiefHeraangemeld()
	{
		return aantalDefinitiefHeraangemeld;
	}

	public void setAantalDefinitiefHeraangemeld(long aantalDefinitiefHeraangemeld)
	{
		this.aantalDefinitiefHeraangemeld = aantalDefinitiefHeraangemeld;
	}

	public long getAantalEenmaligHeraangemeld()
	{
		return aantalEenmaligHeraangemeld;
	}

	public void setAantalEenmaligHeraangemeld(long aantalEenmaligHeraangemeld)
	{
		this.aantalEenmaligHeraangemeld = aantalEenmaligHeraangemeld;
	}

	public long getAantalUitnodigingenUitstrijkje()
	{
		return aantalUitnodigingenUitstrijkje;
	}

	public void setAantalUitnodigingenUitstrijkje(long aantalUitnodigingenUitstrijkje)
	{
		this.aantalUitnodigingenUitstrijkje = aantalUitnodigingenUitstrijkje;
	}

	public long getAantalUitnodigingenZas()
	{
		return aantalUitnodigingenZas;
	}

	public void setAantalUitnodigingenZas(long aantalUitnodigingenZas)
	{
		this.aantalUitnodigingenZas = aantalUitnodigingenZas;
	}

	public long getAantalInVervolgonderzoek()
	{
		return aantalInVervolgonderzoek;
	}

	public void setAantalInVervolgonderzoek(long aantalInVervolgonderzoek)
	{
		this.aantalInVervolgonderzoek = aantalInVervolgonderzoek;
	}

	public long getAantalRondenGesloten()
	{
		return aantalRondenGesloten;
	}

	public void setAantalRondenGesloten(long aantalRondenGesloten)
	{
		this.aantalRondenGesloten = aantalRondenGesloten;
	}

	public long getTotaalAantalBrieven()
	{
		return totaalAantalBrieven;
	}

	public void setTotaalAantalBrieven(long totaalAantalBrieven)
	{
		this.totaalAantalBrieven = totaalAantalBrieven;
	}

	public long getTotaalAantalHuisartsberichten()
	{
		return totaalAantalHuisartsberichten;
	}

	public void setTotaalAantalHuisartsberichten(long totaalHuisartsberichten)
	{
		this.totaalAantalHuisartsberichten = totaalHuisartsberichten;
	}
}
