package nl.rivm.screenit.model;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import javax.persistence.CascadeType;
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

import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.topicuszorg.hibernate.object.annot.ExactValue;
import nl.topicuszorg.organisatie.model.Medewerker;
import nl.topicuszorg.yubikey.model.YubiKey;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(
	indexes = {
		@Index(name = "IDX_GEBRUIKER_ACTIEF", columnList = "actief"),
		@Index(name = "IDX_GEBRUIKER_ACHTERNAAM", columnList = "achternaam") })
@Audited
public class Gebruiker extends Medewerker<InstellingGebruiker> implements Account, IActief
{
	private static final long serialVersionUID = 1L;

	@Enumerated(EnumType.STRING)
	private Aanhef aanhef;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Functie functie;

	@ManyToOne(fetch = FetchType.LAZY)
	@NotAudited
	private Titel titel;

	private String telefoonnummerwerk;

	private String emailwerk;

	private String telefoonnummerextra;

	private String emailextra;

	private String telefoonnummerprive;

	@Temporal(TemporalType.DATE)
	private Date geboortedatum;

	@Column(unique = true, nullable = false)
	@ExactValue
	private String gebruikersnaam;

	private String wachtwoord;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefVanaf;

	@Temporal(TemporalType.TIMESTAMP)
	private Date actiefTotEnMet;

	@Column(unique = true, nullable = false)
	private Integer medewerkercode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date tijdLaatsteFoutieveInlog;

	private Integer foutieveInlogpogingen;

	private InlogStatus inlogstatus;

	@Temporal(TemporalType.TIMESTAMP)
	private Date laatsteKeerWachtwoordGewijzigd;

	private String wachtwoordChangeCode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumWachtwoordAanvraag;

	private Boolean actief;

	private Boolean zorgverlener;

	@Column(length = HibernateMagicNumber.L25)
	private String patholoogId;

	@ManyToOne(cascade = CascadeType.ALL)
	@NotAudited
	private YubiKey yubiKey;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private InlogMethode inlogMethode;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "gebruiker")
	@NotAudited
	private List<AfgeslotenMedewerkerOvereenkomst> afgeslotenKwaliteitsOvereenkomsten;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "gebruiker", cascade = CascadeType.REMOVE)
	private List<GebruikerNieuwsItem> gebruikerNieuwsItems;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	private UploadDocument handtekening;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String ondertekenaar;

	public String getGebruikersnaam()
	{
		return gebruikersnaam;
	}

	public void setGebruikersnaam(String gebruikersnaam)
	{
		this.gebruikersnaam = gebruikersnaam;
	}

	public String getWachtwoord()
	{
		return wachtwoord;
	}

	public void setWachtwoord(String wachtwoord)
	{
		this.wachtwoord = wachtwoord;
	}

	public Date getActiefTotEnMet()
	{
		return actiefTotEnMet;
	}

	public void setActiefTotEnMet(Date einddatum)
	{
		this.actiefTotEnMet = einddatum;
	}

	public Date getTijdLaatsteFoutieveInlog()
	{
		return tijdLaatsteFoutieveInlog;
	}

	public void setTijdLaatsteFoutieveInlog(Date tijdLaatsteFoutieveInlog)
	{
		this.tijdLaatsteFoutieveInlog = tijdLaatsteFoutieveInlog;
	}

	public Integer getFoutieveInlogpogingen()
	{
		return foutieveInlogpogingen;
	}

	public void setFoutieveInlogpogingen(Integer foutieveInlogpogingen)
	{
		this.foutieveInlogpogingen = foutieveInlogpogingen;
	}

	public InlogStatus getInlogstatus()
	{
		return inlogstatus;
	}

	public void setInlogstatus(InlogStatus inlogstatus)
	{
		this.inlogstatus = inlogstatus;
	}

	public Date getLaatsteKeerWachtwoordGewijzigd()
	{
		return laatsteKeerWachtwoordGewijzigd;
	}

	public void setLaatsteKeerWachtwoordGewijzigd(Date laatsteKeerWachtwoordGewijzigd)
	{
		this.laatsteKeerWachtwoordGewijzigd = laatsteKeerWachtwoordGewijzigd;
	}

	public String getWachtwoordChangeCode()
	{
		return wachtwoordChangeCode;
	}

	public void setWachtwoordChangeCode(String wachtwoordChangeCode)
	{
		this.wachtwoordChangeCode = wachtwoordChangeCode;
	}

	public Date getDatumWachtwoordAanvraag()
	{
		return datumWachtwoordAanvraag;
	}

	public void setDatumWachtwoordAanvraag(Date datumWachtwoordAanvraag)
	{
		this.datumWachtwoordAanvraag = datumWachtwoordAanvraag;
	}

	public Aanhef getAanhef()
	{
		return aanhef;
	}

	public void setAanhef(Aanhef aanhef)
	{
		this.aanhef = aanhef;
	}

	public Titel getTitel()
	{
		return titel;
	}

	public void setTitel(Titel titel)
	{
		this.titel = titel;
	}

	public String getTelefoonnummerwerk()
	{
		return telefoonnummerwerk;
	}

	public void setTelefoonnummerwerk(String telefoonnummerwerk)
	{
		this.telefoonnummerwerk = telefoonnummerwerk;
	}

	public String getEmailwerk()
	{
		return emailwerk;
	}

	public void setEmailwerk(String emailwerk)
	{
		this.emailwerk = emailwerk;
	}

	public String getTelefoonnummerextra()
	{
		return telefoonnummerextra;
	}

	public void setTelefoonnummerextra(String telefoonnummerextra)
	{
		this.telefoonnummerextra = telefoonnummerextra;
	}

	public String getEmailextra()
	{
		return emailextra;
	}

	public void setEmailextra(String emailextra)
	{
		this.emailextra = emailextra;
	}

	public String getTelefoonnummerprive()
	{
		return telefoonnummerprive;
	}

	public void setTelefoonnummerprive(String telefoonnummerprive)
	{
		this.telefoonnummerprive = telefoonnummerprive;
	}

	@Override
	public Boolean getActief()
	{
		return actief;
	}

	@Override
	public void setActief(Boolean actief)
	{
		this.actief = actief;
	}

	@Override
	@Transient
	public String getNaamVolledig()
	{
		StringBuffer naamVolledig = new StringBuffer();

		naamVolledig.append(getAchternaam());
		naamVolledig.append(", ");

		if (StringUtils.isNotBlank(getVoorletters()))
		{
			naamVolledig.append(getVoorletters());
			naamVolledig.append(" ");
		}
		else if (StringUtils.isNotBlank(getVoornaam()))
		{
			naamVolledig.append(getVoornaam());
			naamVolledig.append(" ");
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(getTussenvoegsel());
			naamVolledig.append(" ");
		}

		return naamVolledig.toString();
	}

	@Transient
	public String getNaamVolledigMetVoornaam()
	{
		StringBuilder naamVolledig = new StringBuilder();

		naamVolledig.append(getAchternaam());

		if (StringUtils.isNotBlank(getVoorletters()))
		{
			naamVolledig.append(", ");
			naamVolledig.append(getVoorletters());
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(StringUtils.isNotBlank(getVoorletters()) ? " " : ", ");
			naamVolledig.append(getTussenvoegsel());
		}

		if (StringUtils.isNotBlank(getVoornaam()))
		{
			naamVolledig.append(" (");
			naamVolledig.append(getVoornaam());
			naamVolledig.append(")");
		}

		return naamVolledig.toString();
	}

	@Transient
	public String getVoornaamAchternaam()
	{
		StringBuffer voornaamAchternaam = new StringBuffer();
		if (StringUtils.isNotBlank(getVoornaam()))
		{
			voornaamAchternaam.append(getVoornaam());
			voornaamAchternaam.append(" ");
		}
		else if (StringUtils.isNotBlank(getVoorletters()))
		{
			voornaamAchternaam.append(getVoorletters());
			voornaamAchternaam.append(" ");
		}

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			voornaamAchternaam.append(getTussenvoegsel());
			voornaamAchternaam.append(" ");
		}

		voornaamAchternaam.append(getAchternaam());

		return voornaamAchternaam.toString();
	}

	@Transient
	public String getAchternaamVolledig()
	{
		StringBuffer naamVolledig = new StringBuffer();

		naamVolledig.append(getAchternaam());
		naamVolledig.append(", ");

		if (StringUtils.isNotBlank(getTussenvoegsel()))
		{
			naamVolledig.append(getTussenvoegsel());
			naamVolledig.append(" ");
		}

		return naamVolledig.toString();
	}

	public Functie getFunctie()
	{
		return functie;
	}

	public void setFunctie(Functie functie)
	{
		this.functie = functie;
	}

	public Boolean getZorgverlener()
	{
		return zorgverlener;
	}

	public void setZorgverlener(Boolean zorgverlener)
	{
		this.zorgverlener = zorgverlener;
	}

	public String getPatholoogId()
	{
		return patholoogId;
	}

	public void setPatholoogId(String patholoogId)
	{
		this.patholoogId = patholoogId;
	}

	public YubiKey getYubiKey()
	{
		return yubiKey;
	}

	public void setYubiKey(YubiKey yubiKey)
	{
		this.yubiKey = yubiKey;
	}

	public InlogMethode getInlogMethode()
	{
		return inlogMethode;
	}

	public void setInlogMethode(InlogMethode inlogMethode)
	{
		this.inlogMethode = inlogMethode;
	}

	@Override
	public int hashCode()
	{
		final int prime = 31;
		int result = 1;
		if (getId() != null)
		{
			result = prime * result + getId().hashCode();
		}
		else
		{
			result = prime * result + (actief == null ? 0 : actief.hashCode());
			result = prime * result + (getNaamVolledig() == null ? 0 : getNaamVolledig().hashCode());
			result = prime * result + (getUzinummer() == null ? 0 : getUzinummer().hashCode());
			result = prime * result + (getGebruikersnaam() == null ? 0 : getGebruikersnaam().hashCode());
		}
		return result;
	}

	@Override
	public boolean equals(Object obj)
	{
		boolean returnValue = true;
		if (obj == null)
		{
			returnValue = false;
		}
		else if (Hibernate.getClass(this) != Hibernate.getClass(obj))
		{
			returnValue = false;
		}
		else
		{
			Gebruiker other = (Gebruiker) obj;
			if (getId() == null)
			{
				if (other.getId() != null)
				{
					returnValue = false;
				}
			}
			else if (!getId().equals(other.getId()))
			{
				returnValue = false;
			}

			if (returnValue && getId() == null)
			{
				if (actief == null)
				{
					if (other.actief != null)
					{
						returnValue = false;
					}
				}
				else if (!actief.equals(other.actief))
				{
					returnValue = false;
				}
				if (getNaamVolledig() == null)
				{
					if (other.getNaamVolledig() != null)
					{
						returnValue = false;
					}
				}
				else if (!getNaamVolledig().equals(other.getNaamVolledig()))
				{
					returnValue = false;
				}
				if (getGebruikersnaam() == null)
				{
					if (other.getGebruikersnaam() != null)
					{
						returnValue = false;
					}
				}
				else if (!getGebruikersnaam().equals(other.getGebruikersnaam()))
				{
					returnValue = false;
				}
				if (getUzinummer() == null)
				{
					if (other.getUzinummer() != null)
					{
						returnValue = false;
					}
				}
				else if (!getUzinummer().equals(other.getUzinummer()))
				{
					returnValue = false;
				}
			}
		}

		return returnValue;
	}

	public Date getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(Date geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public List<AfgeslotenMedewerkerOvereenkomst> getAfgeslotenKwaliteitsOvereenkomsten()
	{
		return afgeslotenKwaliteitsOvereenkomsten;
	}

	public void setAfgeslotenKwaliteitsOvereenkomsten(List<AfgeslotenMedewerkerOvereenkomst> afgeslotenKwaliteitsOvereenkomsten)
	{
		this.afgeslotenKwaliteitsOvereenkomsten = afgeslotenKwaliteitsOvereenkomsten;
	}

	public List<GebruikerNieuwsItem> getGebruikerNieuwsItems()
	{
		return gebruikerNieuwsItems;
	}

	public void setGebruikerNieuwsItems(List<GebruikerNieuwsItem> gebruikerNieuwsItems)
	{
		this.gebruikerNieuwsItems = gebruikerNieuwsItems;
	}

	public UploadDocument getHandtekening()
	{
		return handtekening;
	}

	public void setHandtekening(UploadDocument handtekening)
	{
		this.handtekening = handtekening;
	}

	public String getOndertekenaar()
	{
		return ondertekenaar;
	}

	public void setOndertekenaar(String ondertekenaar)
	{
		this.ondertekenaar = ondertekenaar;
	}

	public Date getActiefVanaf()
	{
		return actiefVanaf;
	}

	public void setActiefVanaf(Date actiefVanaf)
	{
		this.actiefVanaf = actiefVanaf;
	}

	public Integer getMedewerkercode()
	{
		return medewerkercode;
	}

	public void setMedewerkercode(Integer medewerkercode)
	{
		this.medewerkercode = medewerkercode;
	}
}
