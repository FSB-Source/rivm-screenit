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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.Index;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;
import javax.xml.bind.annotation.XmlTransient;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.model.helper.HibernateMagicNumber;
import nl.rivm.screenit.model.nieuws.GebruikerNieuwsItem;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.topicuszorg.hibernate.object.annot.ExactValue;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.NaamGebruik;
import nl.topicuszorg.yubikey.model.YubiKey;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Hibernate;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.CascadeType;
import org.hibernate.annotations.Proxy;
import org.hibernate.envers.Audited;
import org.hibernate.envers.NotAudited;

@Entity
@Table(schema = "algemeen", name = "org_medewerker",
	indexes = {
		@Index(name = "IDX_GEBRUIKER_ACTIEF", columnList = "actief"),
		@Index(name = "IDX_GEBRUIKER_ACHTERNAAM", columnList = "achternaam") })
@Proxy
@Cache(
	usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE,
	region = "organisatie.cache"
)
@Audited
@Getter
@Setter
public class Gebruiker extends SingleTableHibernateObject implements Account, IActief
{
	@Column(
		unique = true,
		nullable = true,
		length = 8
	)
	private String agbcode;

	@Column(
		unique = true,
		nullable = true,
		length = 11
	)
	private String bignummer;

	@Column(
		unique = true,
		nullable = true,
		length = 9
	)
	private String uzinummer;

	@Deprecated(forRemoval = true)
	@Column(
		unique = true,
		nullable = true,
		length = 10
	)
	private String pasnummer;

	@Deprecated(forRemoval = true)
	@Column(
		nullable = true,
		length = 8
	)
	private String rolCode;

	@Deprecated(forRemoval = true)
	@Column(
		unique = true,
		nullable = true,
		length = 100
	)
	private String uziPasSerial;

	@Column(
		length = 20
	)
	private String voorletters;

	@Column(
		length = 20
	)
	private String tussenvoegsel;

	@Column(
		length = 50
	)
	private String voornaam;

	@Column(
		nullable = false,
		length = 50
	)
	private String achternaam;

	@OneToMany(
		mappedBy = "medewerker"
	)
	@Cache(
		usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE,
		region = "organisatie.cache"
	)
	@XmlTransient
	private List<InstellingGebruiker> organisatieMedewerkers = new ArrayList<>();

	@ManyToMany(
		fetch = FetchType.LAZY,
		cascade = { javax.persistence.CascadeType.PERSIST, javax.persistence.CascadeType.MERGE }
	)
	@Cascade({ CascadeType.PERSIST, CascadeType.SAVE_UPDATE })
	@Cache(
		usage = CacheConcurrencyStrategy.NONSTRICT_READ_WRITE,
		region = "organisatie.cache"
	)
	private List<Adres> adressen = new ArrayList<>();

	@Deprecated(forRemoval = true)
	@Column(
		length = 200,
		nullable = true
	)
	private String partnerAchternaam;

	@Deprecated(forRemoval = true)
	@Column(
		length = 10,
		nullable = true
	)
	private String partnerTussenvoegsel;

	@Deprecated(forRemoval = true)
	@Enumerated(EnumType.STRING)
	private NaamGebruik naamGebruik;

	@Deprecated(forRemoval = true)
	@Column(
		nullable = true
	)
	private String volledigenaamweergave;

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

	@Column(nullable = false)
	private boolean wachtwoordVerlooptWaarschuwingVerzonden;

	private String wachtwoordChangeCode;

	@Temporal(TemporalType.TIMESTAMP)
	private Date datumWachtwoordAanvraag;

	private Boolean actief;

	private Boolean zorgverlener;

	@Column(length = HibernateMagicNumber.L25)
	private String patholoogId;

	@ManyToOne(cascade = javax.persistence.CascadeType.ALL)
	@NotAudited
	private YubiKey yubiKey;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private InlogMethode inlogMethode;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "gebruiker")
	@NotAudited
	private List<AfgeslotenMedewerkerOvereenkomst> afgeslotenKwaliteitsOvereenkomsten;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "gebruiker", cascade = javax.persistence.CascadeType.REMOVE)
	private List<GebruikerNieuwsItem> gebruikerNieuwsItems;

	@OneToOne(fetch = FetchType.LAZY, optional = true)
	private UploadDocument handtekening;

	@Column(nullable = true, length = HibernateMagicNumber.L255)
	private String ondertekenaar;

	public Gebruiker()
	{
		this.naamGebruik = NaamGebruik.EIGEN;
	}

	@Override
	public String toString()
	{
		return this.getNaamVolledig();
	}

	@Deprecated(forRemoval = true)
	public Adres getHuidigAdres()
	{
		Adres adres = null;
		if (this.adressen != null && !this.adressen.isEmpty())
		{
			adres = this.adressen.get(this.adressen.size() - 1);
		}

		return adres;
	}

	@Deprecated(forRemoval = true)
	public Adres add(Adres adres)
	{
		if (this.adressen == null)
		{
			this.adressen = new ArrayList();
		}

		this.adressen.add(adres);
		return adres;
	}

	@Deprecated(forRemoval = true)
	public String getStraat()
	{
		Adres adres = this.getHuidigAdres();
		String straat = null;
		if (adres != null)
		{
			straat = adres.getStraat();
		}

		return straat;
	}

	@Deprecated(forRemoval = true)
	public Integer getHuisnummer()
	{
		Adres adres = this.getHuidigAdres();
		Integer huisnummer = null;
		if (adres != null)
		{
			huisnummer = adres.getHuisnummer();
		}

		return huisnummer;
	}

	@Deprecated(forRemoval = true)
	public String getHuisnummerToevoeging()
	{
		Adres adres = this.getHuidigAdres();
		String huisnummertoevoeging = null;
		if (adres != null)
		{
			huisnummertoevoeging = adres.getHuisnummerToevoeging();
		}

		return huisnummertoevoeging;
	}

	@Deprecated(forRemoval = true)
	public String getPostcode()
	{
		Adres adres = this.getHuidigAdres();
		String postcode = null;
		if (adres != null)
		{
			postcode = adres.getPostcode();
		}

		return postcode;
	}

	@Deprecated(forRemoval = true)
	public String getPlaats()
	{
		Adres adres = this.getHuidigAdres();
		String plaats = null;
		if (adres != null)
		{
			plaats = adres.getPlaats();
		}

		return plaats;
	}

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

}
