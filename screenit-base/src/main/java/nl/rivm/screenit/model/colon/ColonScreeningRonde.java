package nl.rivm.screenit.model.colon;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.OnbekendeHuisarts;
import nl.rivm.screenit.model.ScreeningRonde;
import nl.rivm.screenit.model.colon.enums.ColonDefinitiefVervolgbeleid;

import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

import static org.hibernate.envers.RelationTargetAuditMode.NOT_AUDITED;

@Entity
@Table(schema = "colon", indexes = { @Index(name = "idx_COLON_SCREENING_RONDE_STATUS", columnList = "status"),
	@Index(name = "idx_COLON_SCREENING_RONDE_definitiefVervolgbeleid", columnList = "definitiefVervolgbeleid") })
@Audited
public class ColonScreeningRonde extends ScreeningRonde<ColonDossier, ColonBrief, ColonAfmelding, ColonUitnodiging>
{
	private static final long serialVersionUID = 1L;

	@OneToMany(mappedBy = "colonScreeningRonde", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<IFOBTTest> ifobtTesten = new ArrayList<>();

	@OneToOne(optional = true, cascade = CascadeType.ALL)
	private IFOBTTest laatsteIFOBTTest;

	@Deprecated
	@OneToOne(optional = true, cascade = CascadeType.ALL)
	private IFOBTTest laatsteIFOBTTestExtra;

	@ManyToOne(fetch = FetchType.LAZY, optional = true)
	@Audited(targetAuditMode = NOT_AUDITED)
	@Cascade(org.hibernate.annotations.CascadeType.SAVE_UPDATE)
	private EnovationHuisarts colonHuisarts;

	@Deprecated
	@OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, optional = true)
	@Audited(targetAuditMode = NOT_AUDITED)
	private OnbekendeHuisarts onbekendeHuisarts;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumVastleggenHuisarts;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "colonScreeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonIntakeAfspraak> afspraken = new ArrayList<>();

	@ManyToOne(cascade = CascadeType.ALL)
	private ColonIntakeAfspraak laatsteAfspraak;

	@OneToMany(mappedBy = "screeningRonde", cascade = CascadeType.ALL)
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonVerslag> verslagen = new ArrayList<>();

	@ManyToOne(optional = false)
	private ColonDossier dossier;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonUitnodiging> uitnodigingen = new ArrayList<>();

	@ManyToOne()
	private ColonUitnodiging laatsteUitnodiging;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonBrief> brieven = new ArrayList<>();

	@ManyToOne()
	private ColonBrief laatsteBrief;

	@OneToMany(cascade = CascadeType.ALL, mappedBy = "screeningRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	private List<ColonAfmelding> afmeldingen = new ArrayList<>();

	@OneToOne(optional = true, cascade = CascadeType.ALL)
	private ColonAfmelding laatsteAfmelding;

	@OneToOne(optional = true, cascade = CascadeType.ALL, mappedBy = "ronde")
	private OpenUitnodiging openUitnodiging;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "screeningsRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<ColonHuisartsBericht> huisartsBerichten = new ArrayList<>();

	@Enumerated(EnumType.STRING)
	private ColonDefinitiefVervolgbeleid definitiefVervolgbeleid;

	@OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "screeningsRonde")
	@Cache(usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE, region = "screenit.cache")
	@NotAudited
	private List<ColonKoppelresultaatKankerregistratie> koppelresultatenKankerregistratie = new ArrayList<>();

	private boolean gepusht;

	public List<IFOBTTest> getIfobtTesten()
	{
		return ifobtTesten;
	}

	public void setIfobtTesten(List<IFOBTTest> ifobtTesten)
	{
		this.ifobtTesten = ifobtTesten;
	}

	public IFOBTTest getLaatsteIFOBTTest()
	{
		return laatsteIFOBTTest;
	}

	public void setLaatsteIFOBTTest(IFOBTTest laatsteIFOBTTest)
	{
		this.laatsteIFOBTTest = laatsteIFOBTTest;
	}

	public IFOBTTest getLaatsteIFOBTTestExtra()
	{
		return laatsteIFOBTTestExtra;
	}

	public void setLaatsteIFOBTTestExtra(IFOBTTest laatsteIFOBTTestExtra)
	{
		this.laatsteIFOBTTestExtra = laatsteIFOBTTestExtra;
	}

	public List<ColonIntakeAfspraak> getAfspraken()
	{
		return afspraken;
	}

	public void setAfspraken(List<ColonIntakeAfspraak> afspraken)
	{
		this.afspraken = afspraken;
	}

	public ColonIntakeAfspraak getLaatsteAfspraak()
	{
		return laatsteAfspraak;
	}

	public void setLaatsteAfspraak(ColonIntakeAfspraak laatsteAfspraak)
	{
		this.laatsteAfspraak = laatsteAfspraak;
	}

	public List<ColonVerslag> getVerslagen()
	{
		return verslagen;
	}

	public void setVerslagen(List<ColonVerslag> verslagen)
	{
		this.verslagen = verslagen;
	}

	@Override
	public ColonDossier getDossier()
	{
		return dossier;
	}

	@Override
	public void setDossier(ColonDossier dossier)
	{
		this.dossier = dossier;
	}

	@Override
	public List<ColonUitnodiging> getUitnodigingen()
	{
		return uitnodigingen;
	}

	@Override
	public void setUitnodigingen(List<ColonUitnodiging> uitnodigingen)
	{
		this.uitnodigingen = uitnodigingen;
	}

	@Override
	public ColonUitnodiging getLaatsteUitnodiging()
	{
		return laatsteUitnodiging;
	}

	@Override
	public void setLaatsteUitnodiging(ColonUitnodiging laatsteUitnodiging)
	{
		this.laatsteUitnodiging = laatsteUitnodiging;
	}

	@Override
	public List<ColonBrief> getBrieven()
	{
		return brieven;
	}

	@Override
	public void setBrieven(List<ColonBrief> brieven)
	{
		this.brieven = brieven;
	}

	@Override
	public ColonBrief getLaatsteBrief()
	{
		return laatsteBrief;
	}

	@Override
	public void setLaatsteBrief(ColonBrief laatsteBrief)
	{
		this.laatsteBrief = laatsteBrief;
	}

	@Override
	public List<ColonAfmelding> getAfmeldingen()
	{
		return afmeldingen;
	}

	@Override
	public void setAfmeldingen(List<ColonAfmelding> afmeldingen)
	{
		this.afmeldingen = afmeldingen;
	}

	@Override
	public ColonAfmelding getLaatsteAfmelding()
	{
		return laatsteAfmelding;
	}

	@Override
	public void setLaatsteAfmelding(ColonAfmelding laatsteAfmelding)
	{
		this.laatsteAfmelding = laatsteAfmelding;
	}

	public OpenUitnodiging getOpenUitnodiging()
	{
		return openUitnodiging;
	}

	public void setOpenUitnodiging(OpenUitnodiging openUitnodiging)
	{
		this.openUitnodiging = openUitnodiging;
	}

	public List<ColonHuisartsBericht> getHuisartsBerichten()
	{
		return huisartsBerichten;
	}

	public void setHuisartsBerichten(List<ColonHuisartsBericht> huisartsBerichten)
	{
		this.huisartsBerichten = huisartsBerichten;
	}

	public ColonDefinitiefVervolgbeleid getDefinitiefVervolgbeleid()
	{
		return definitiefVervolgbeleid;
	}

	public void setDefinitiefVervolgbeleid(ColonDefinitiefVervolgbeleid definitiefVervolgbeleid)
	{
		this.definitiefVervolgbeleid = definitiefVervolgbeleid;
	}

	public boolean isGepusht()
	{
		return gepusht;
	}

	public void setGepusht(boolean gepusht)
	{
		this.gepusht = gepusht;
	}

	public List<ColonKoppelresultaatKankerregistratie> getKoppelresultatenKankerregistratie()
	{
		return koppelresultatenKankerregistratie;
	}

	public void setKoppelresultatenKankerregistratie(List<ColonKoppelresultaatKankerregistratie> koppelresultatenKankerregistratie)
	{
		this.koppelresultatenKankerregistratie = koppelresultatenKankerregistratie;
	}

	@Deprecated
	public OnbekendeHuisarts getOnbekendeHuisarts()
	{
		return onbekendeHuisarts;
	}

	@Deprecated
	public void setOnbekendeHuisarts(OnbekendeHuisarts onbekendeHuisarts)
	{
		this.onbekendeHuisarts = onbekendeHuisarts;
	}

	public EnovationHuisarts getColonHuisarts()
	{
		return colonHuisarts;
	}

	public void setColonHuisarts(EnovationHuisarts enovationHuisarts)
	{
		this.colonHuisarts = enovationHuisarts;
	}

	public Date getDatumVastleggenHuisarts()
	{
		return datumVastleggenHuisarts;
	}

	public void setDatumVastleggenHuisarts(Date datumVastleggenHuisarts)
	{
		this.datumVastleggenHuisarts = datumVastleggenHuisarts;
	}
}
