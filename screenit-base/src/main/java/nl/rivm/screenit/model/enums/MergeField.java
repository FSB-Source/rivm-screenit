package nl.rivm.screenit.model.enums;

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

import java.io.IOException;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.function.Supplier;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.model.Aanhef;
import nl.rivm.screenit.model.AfmeldingType;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.CentraleEenheid;
import nl.rivm.screenit.model.ClientBrief;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Functie;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.InpakbareUitnodiging;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.OrganisatieParameterKey;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.Titel;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixHuisarts;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLeeftijdcategorie;
import nl.rivm.screenit.model.cervix.enums.CervixMonsterType;
import nl.rivm.screenit.model.cervix.enums.CervixNietAnalyseerbaarReden;
import nl.rivm.screenit.model.cervix.enums.CervixRedenUitnodiging;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.facturatie.CervixBetaalopdracht;
import nl.rivm.screenit.model.colon.ColonAfmelding;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.model.mamma.MammaLezing;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.model.mamma.enums.MammaAmputatie;
import nl.rivm.screenit.model.mamma.enums.MammaBeperktBeoordeelbaarReden;
import nl.rivm.screenit.model.mamma.enums.MammaMammografieIlmStatus;
import nl.rivm.screenit.model.mamma.enums.MammaOnderzoekStatus;
import nl.rivm.screenit.model.mamma.enums.MammaUitstelReden;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.model.mamma.enums.MammaZijde;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenInstellingOvereenkomst;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.service.BarcodeService;
import nl.rivm.screenit.service.HeraanmeldenMergeVeldService;
import nl.rivm.screenit.service.OrganisatieParameterService;
import nl.rivm.screenit.service.cervix.CervixBaseBetalingService;
import nl.rivm.screenit.service.cervix.impl.CervixMonsterIdBarcode;
import nl.rivm.screenit.service.cervix.impl.CervixMonsterIdLabelBarcode;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;
import nl.rivm.screenit.service.colon.ColonIntakelocatieService;
import nl.rivm.screenit.service.impl.UitnodigingIdBarcode;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;
import nl.rivm.screenit.service.mamma.MammaMergeFieldService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.AfmeldingUtil;
import nl.rivm.screenit.util.BriefUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.rivm.screenit.util.cervix.CervixMonsterUtil;
import nl.rivm.screenit.util.mamma.MammaBeoordelingUtil;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;
import nl.topicuszorg.organisatie.model.Adres;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.util.postcode.PostcodeFormatter;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;
import org.hibernate.envers.query.AuditEntity;
import org.krysalis.barcode4j.impl.AbstractBarcodeBean;
import org.krysalis.barcode4j.impl.fourstate.RoyalMailCBCBean;
import org.springframework.beans.support.PropertyComparator;

import com.google.common.base.Strings;

import static nl.rivm.screenit.Constants.INLINE_ID_SO_LOGO_EMAIL;
import static nl.rivm.screenit.model.enums.MergeFieldFlag.NIET_IN_HUISARTSBERICHT;
import static nl.rivm.screenit.model.enums.MergeFieldFlag.NIET_NAAR_INPAKCENTRUM;
import static nl.rivm.screenit.model.enums.MergeFieldFlag.QR_CODE;
import static nl.rivm.screenit.model.enums.MergeFieldFlag.WAARDE_NIET_TRIMMEN;

@Slf4j
public enum MergeField
{

	UNIEK_BRIEF_KENMERK("_UNIEK_BRIEF_KENMERK", MergeFieldTestType.OVERIGE, String.class, () -> "K6BD83FL", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var brief = context.getBrief();
				var uitnodiging = context.getColonUitnodiging();
				if (brief != null && brief.getId() != null)
				{
					return "K" + Long.toHexString(brief.getId()).toUpperCase();
				}
				else if (uitnodiging != null)
				{
					return "KU" + Long.toHexString(uitnodiging.getUitnodigingsId()).toUpperCase();
				}
				return null;
			}
		},
	SO_ID("_SO_ID")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getId();
				}
				return null;
			}
		},

	SO_NAAM("_SO_NAAM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getNaam();
				}
				return null;
			}
		},

	SO_LOGO("_SO_LOGO", NIET_IN_HUISARTSBERICHT, NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getLogoBrief();
				}
				return null;
			}
		},
	SO_LOGO_EMAIL("_SO_LOGO_EMAIL", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return "<img src='cid:" + INLINE_ID_SO_LOGO_EMAIL + "'>";
			}
		},

	SO_HANDTEKENING_BESTUURDER("_SO_HANDTEKENING_BESTUURDER", NIET_IN_HUISARTSBERICHT, NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getBestuurSign();
				}
				return null;
			}
		},

	SO_HANDTEKENING_RCMDL("_SO_HANDTEKENING_RCMDL", NIET_IN_HUISARTSBERICHT, NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getRcmdlSign();
				}
				return null;
			}
		},

	SO_KWALITEITSLOGO("_SO_KWALITEITSLOGO", NIET_IN_HUISARTSBERICHT, NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getKwaliteitslogo();
				}
				return null;
			}
		},

	SO_POSTADRES("_SO_POSTADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return AdresUtil.getVolledigeAdresString(getAdres(getScreeningOrganisatie(context), 0));
			}
		},

	SO_STRAATNAAM("_SO_STRAATNAAM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 0);
				if (adres != null)
				{
					return adres.getStraat();
				}
				return null;
			}

		},

	SO_HUISNUMMER_TOEV("_SO_HUISNUMMER_TOEV")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 0);
				if (adres != null)
				{
					var adresString = new StringBuilder();
					if (adres.getHuisnummer() != null)
					{
						adresString.append(adres.getHuisnummer());
					}

					if (!Strings.isNullOrEmpty(adres.getHuisletter()))
					{
						adresString.append(" ");
						adresString.append(adres.getHuisletter());
					}

					if (!Strings.isNullOrEmpty(adres.getHuisnummerToevoeging()))
					{
						adresString.append(" ");
						adresString.append(adres.getHuisnummerToevoeging());
					}

					if (adres.getHuisnummer() == null && !Strings.isNullOrEmpty(adres.getHuisnummerAanduiding()))
					{

						adresString.append(" ");
						adresString.append(adres.getHuisnummerAanduiding());
					}

					return adresString.toString();
				}
				return null;
			}

		},

	SO_POSTCODE("_SO_POSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 0);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}

		},

	SO_PLAATS("_SO_PLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 0);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}

		},

	SO_POSTBUSNR("_SO_POSTBUSNR")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 1);
				if (adres != null)
				{
					return adres.getHuisnummer();
				}
				return null;
			}
		},

	SO_POSTBUSNR_DK("_SO_POSTBUSNR_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return adres.getHuisnummer();
						}
					}
				}
				return null;
			}
		},

	SO_POSTBUSNR_BMHK("_SO_POSTBUSNR_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return adres.getHuisnummer();
						}
					}
				}
				return null;
			}
		},

	MAMMA_CE_POSTBUSNR("_BK_CE_POSTBUSNR", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 1);
				if (adres != null)
				{
					return adres.getHuisnummer();
				}
				return null;
			}
		},

	SO_POSTBUSPOSTCODE("_SO_POSTBUSPOSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 1);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}
		},

	SO_POSTBUSPOSTCODE_DK("_SO_POSTBUSPOSTCODE_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
						}
					}
				}
				return null;
			}
		},

	SO_POSTBUSPOSTCODE_BMHK("_SO_POSTBUSPOSTCODE_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
						}
					}
				}
				return null;
			}
		},

	MAMMA_CE_POSTBUSPOSTCODE("_BK_CE_POSTBUSPOSTCODE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 1);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}
		},

	SO_POSTBUSPLAATS("_SO_POSTBUSPLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 1);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}
		},

	SO_POSTBUSPLAATS_DK("_SO_POSTBUSPLAATS_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return adres.getPlaats();
						}
					}
				}
				return null;
			}
		},

	SO_POSTBUSPLAATS_BMHK("_SO_POSTBUSPLAATS_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						var adres = contactGegevens.getPostbusnummerAdres();
						if (adres != null)
						{
							return adres.getPlaats();
						}
					}
				}
				return null;
			}
		},

	MAMMA_CE_POSTBUSPLAATS("_BK_CE_POSTBUSPLAATS", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 1);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}
		},

	SO_ANTWOORDNR("_SO_ANTWOORDNR")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 2);
				if (adres != null)
				{
					return adres.getHuisnummer();
				}
				return null;
			}
		},

	SO_ANTWOORDNR_DK("_SO_ANTWOORDNR_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensDk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return adres.getHuisnummer();
					}
				}
				return null;
			}
		},

	SO_ANTWOORDNR_BMHK("_SO_ANTWOORDNR_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensBmhk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return adres.getHuisnummer();
					}
				}
				return null;
			}
		},

	MAMMA_CE_ANTWOORDNR("_BK_CE_ANTWOORDNR", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 2);
				if (adres != null)
				{
					return adres.getHuisnummer();
				}
				return null;
			}
		},

	SO_ANTWOORDNRPOSTCODE("_SO_ANTWOORDNRPOSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 2);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}
		},

	SO_ANTWOORDNRPOSTCODE_DK("_SO_ANTWOORDNRPOSTCODE_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensDk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}
		},

	SO_ANTWOORDNRPOSTCODE_BMHK("_SO_ANTWOORDNRPOSTCODE_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensBmhk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}
		},

	MAMMA_CE_ANTWOORDNRPOSTCODE("_BK_CE_ANTWOORDNRPOSTCODE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 2);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}
		},

	SO_ANTWOORDNRPLAATS("_SO_ANTWOORDNRPLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getScreeningOrganisatie(context), 2);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}
		},

	SO_ANTWOORDNRPLAATS_DK("_SO_ANTWOORDNRPLAATS_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensDk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return adres.getPlaats();
					}
				}
				return null;
			}
		},

	SO_ANTWOORDNRPLAATS_BMHK("_SO_ANTWOORDNRPLAATS_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var contactGegevens = MergeField.getScreeningOrganisatie(context).getRegioBvoContactGegevensBmhk();
				if (contactGegevens != null)
				{
					var adres = contactGegevens.getAntwoordnummerAdres();
					if (adres != null)
					{
						return adres.getPlaats();
					}
				}
				return null;
			}
		},

	MAMMA_CE_ANTWOORDNRPLAATS("_BK_CE_ANTWOORDNRPLAATS", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getMammaCentraleEenheid(context), 2);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}
		},

	SO_ANTWOORDNUMMER_BMHK_LAB(
		"_SO_ANTWOORDNUMMER_BMHK_LAB",
		MergeFieldTestType.BMHKLAB,
		"bmhkLaboratorium.retouradressen[0].adres.huisnummer",
		Integer.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				String antwoordnummer = null;
				var bmhkRetouradres = getBmhkRetouradres(context.getCervixUitnodiging());
				if (bmhkRetouradres != null && bmhkRetouradres.getHuisnummer() != null)
				{
					antwoordnummer = bmhkRetouradres.getHuisnummer().toString();
				}
				return antwoordnummer;
			}

		},

	SO_ANTWNRPOSTCODE_BMHK_LAB(
		"_SO_ANTWNRPOSTCODE_BMHK_LAB",
		MergeFieldTestType.BMHKLAB,
		"bmhkLaboratorium.retouradressen[0].adres.postcode",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				String postcode = null;
				var bmhkRetouradres = getBmhkRetouradres(context.getCervixUitnodiging());
				if (bmhkRetouradres != null)
				{
					postcode = PostcodeFormatter.formatPostcode(bmhkRetouradres.getPostcode(), true);
				}
				return postcode;
			}

		},

	SO_ANTWNRPLAATS_BMHK_LAB(
		"_SO_ANTWNRPLAATS_BMHK_LAB",
		MergeFieldTestType.BMHKLAB,
		"bmhkLaboratorium.retouradressen[0].adres.plaats",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				String plaats = null;
				var bmhkRetouradres = getBmhkRetouradres(context.getCervixUitnodiging());
				if (bmhkRetouradres != null)
				{
					plaats = bmhkRetouradres.getPlaats();
				}
				return plaats;
			}

		},

	SO_ANTWNRKIX_BMHK_LAB(
		"_SO_ANTWNRKIX_BMHK_LAB",
		RoyalMailCBCBean.class,
		null,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return AdresUtil.createKixCode(getBmhkRetouradres(context.getCervixUitnodiging()));
			}

		},

	SO_TEL("_SO_TEL")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getTelefoon();
				}
				return null;
			}
		},

	SO_TEL_DK("_SO_TEL_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						return contactGegevens.getTelefoon();
					}
				}
				return null;
			}
		},

	SO_TEL_BMHK("_SO_TEL_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						return contactGegevens.getTelefoon();
					}
				}
				return null;
			}
		},

	MAMMA_CE_TEL_INFOLIJN("_BK_CE_TEL_INFOLIJN", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getTelefoon();
				}
				return null;
			}
		},
	MAMMA_CE_TEL_PLANNING("_BK_CE_TEL_PLANNING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getTelefoon2();
				}
				return null;
			}
		},
	MAMMA_CE_TEL_MINDER_VALIDE("_BK_CE_TEL_MINDER_VALIDE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getTelefoon4();
				}
				return null;
			}
		},

	MAMMA_CE_TEL_PROF("_BK_CE_TEL_PROF", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getTelefoon3();
				}
				return null;
			}
		},

	SO_TEL2("_SO_TEL2")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getTelefoon2();
				}
				return null;
			}

		},

	SO_FAX("_SO_FAX")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getFax();
				}
				return null;
			}
		},

	SO_EMAILDRES("_SO_EMAILADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getEmail();
				}
				return null;
			}
		},

	SO_EMAILADRES_DK("_SO_EMAILADRES_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						return contactGegevens.getEmail();
					}
				}
				return null;
			}
		},

	SO_EMAILADRES_BMHK("_SO_EMAILADRES_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						return contactGegevens.getEmail();
					}
				}
				return null;
			}
		},

	MAMMA_CE_EMAIL_INFOLIJN("_BK_CE_EMAIL_INFOLIJN", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getEmail();
				}
				return null;
			}
		},

	MAMMA_CE_EMAIL_PLANNING("_BK_CE_EMAIL_PLANNING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getEmail2();
				}
				return null;
			}
		},

	MAMMA_CE_EMAIL_MINDER_VALIDE("_BK_CE_EMAIL_MINDER_VALIDE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getEmail4();
				}
				return null;
			}
		},

	MAMMA_CE_EMAIL_PROF("_BK_CE_EMAIL_PROF", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getEmail3();
				}
				return null;
			}
		},

	SO_WEBSITE("_SO_WEBSITEADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getWebsite();
				}
				return null;
			}
		},

	SO_RCMDL("_SO_RCMDL")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getRcmdl();
				}
				return null;
			}
		},

	SO_RECHTBANK("_SO_RECHTBANK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getRechtbank();
				}
				return null;
			}

		},

	SO_OPENINGSTIJDEN_TEKST("_SO_OPENINGSTIJDEN_TEKST")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getClientPortaalVrijeTekst();
				}
				return null;
			}
		},

	SO_OPENINGSTIJDEN_TEKST_DK("_SO_OPENINGSTIJDEN_TEKST_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensDk();
					if (contactGegevens != null)
					{
						return contactGegevens.getClientPortaalVrijeTekst();
					}
				}
				return null;
			}
		},

	SO_OPENINGSTIJDEN_TEKST_BMHK("_SO_OPENINGSTIJDEN_TEKST_BMHK", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					var contactGegevens = screeningOrganisatie.getRegioBvoContactGegevensBmhk();
					if (contactGegevens != null)
					{
						return contactGegevens.getClientPortaalVrijeTekst();
					}
				}
				return null;
			}
		},

	MAMMA_CE_OPENINGSTIJDEN_TEKST("_BK_CE_OPENINGSTIJDEN_TEKST", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var centraleEenheid = getMammaCentraleEenheid(context);
				if (centraleEenheid != null)
				{
					return centraleEenheid.getClientPortaalVrijeTekst();
				}
				return null;
			}
		},

	SO_IBAN("_SO_IBAN")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getIban();
				}
				return null;
			}
		},

	SO_TENAAMSTELLING("_SO_TENAAMSTELLING")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getIbanTenaamstelling();
				}
				return null;
			}
		},

	CLIENT_BSN("_CLIENT_BSN", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return context.getClient().getPersoon().getBsn();
				}
				return null;
			}
		},

	CLIENT_NAAM("_CLIENT_NAAM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return NaamUtil.voorlettersTussenvoegselEnAanspreekAchternaam(context.getClient());
				}
				return null;
			}
		},

	CLIENT_ADRES("_CLIENT_ADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return AdresUtil.getAdres(getClientAdres(context));
				}
				return null;
			}
		},

	CLIENT_POSTCODE("_CLIENT_POSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return PostcodeFormatter.formatPostcode(getClientAdres(context).getPostcode(), true);
				}
				return null;
			}
		},

	CLIENT_WOONPLAATS("_CLIENT_WOONPLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return getClientAdres(context).getPlaats();
				}
				return null;
			}
		},

	KIX_CLIENT("_CLIENT_KIX", RoyalMailCBCBean.class, null)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null)
				{
					return AdresUtil.createKixCode(getClientAdres(context));
				}
				return null;
			}

		},

	CLIENT_GEBOORTEDATUM("_CLIENT_GEBOORTEDATUM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return DateUtil.getGeboortedatum(context.getClient());
			}

		},

	CLIENT_AANHEF("_CLIENT_AANHEF")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return NaamUtil.getGewensteAanspreekVorm(context.getClient());
			}

		},

	FORM_NUMMER("_FORM_NUMMER", MergeFieldTestType.OVERIGE, String.class, () -> "12345", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{

				return null;
			}

		},

	IL_NAAM("_IL_NAAM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakelocatie() != null)
				{
					return context.getIntakelocatie().getNaam();
				}

				if (context.getIntakeAfspraak() != null)
				{
					return context.getIntakeAfspraak().getKamer().getIntakelocatie().getNaam();
				}

				return null;
			}
		},

	IL_ADRES("_IL_ADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return AdresUtil.getVolledigeAdresString(getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 0));
				}
				return null;
			}
		},

	IL_STRAATNAAM("_IL_STRAATNAAM")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 0);
					if (adres != null)
					{
						return adres.getStraat();
					}
				}
				return null;
			}
		},

	IL_HUISNUMMER_TOEV("_IL_HUISNUMMER_TOEV")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 0);
					if (adres != null)
					{
						var adresString = new StringBuilder();
						if (adres.getHuisnummer() != null)
						{
							adresString.append(adres.getHuisnummer());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisletter()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisletter());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisnummerToevoeging()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisnummerToevoeging());
						}

						if (adres.getHuisnummer() == null && !Strings.isNullOrEmpty(adres.getHuisnummerAanduiding()))
						{

							adresString.append(" ");
							adresString.append(adres.getHuisnummerAanduiding());
						}

						return adresString.toString();
					}
				}
				return null;
			}

		},

	IL_POSTCODE("_IL_POSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 0);
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}

		},

	IL_PLAATS("_IL_PLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 0);
					if (adres != null)
					{
						return adres.getPlaats();
					}
				}
				return null;
			}

		},

	IL_POSTBUSNR("_IL_POSTBUSNR")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 1);
					if (adres != null)
					{
						return adres.getHuisnummer();
					}
				}
				return null;
			}

		},

	IL_POSTBUSPOSTCODE("_IL_POSTBUSPOSTCODE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 1);
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}

		},

	IL_POSTBUSPLAATS("_IL_POSTBUSPLAATS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var adres = getAdres(context.getIntakeAfspraak().getKamer().getIntakelocatie(), 1);
					if (adres != null)
					{
						return adres.getPlaats();
					}
				}
				return null;
			}

		},

	IL_TEL("_IL_TEL")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return context.getIntakeAfspraak().getKamer().getIntakelocatie().getTelefoon();
				}
				return null;
			}

		},

	IL_FAX("_IL_FAX")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return context.getIntakeAfspraak().getKamer().getIntakelocatie().getFax();
				}
				return null;
			}

		},

	IL_EMAILADRES("_IL_EMAILADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return context.getIntakeAfspraak().getKamer().getIntakelocatie().getEmail();
				}
				return null;
			}

		},

	IL_WEBSITEADRES("_IL_WEBSITEADRES")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return context.getIntakeAfspraak().getKamer().getIntakelocatie().getWebsite();
				}
				return null;
			}

		},

	IL_LOKATIE("_IL_LOKATIE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return getOrganisatieParameterService().getOrganisatieParameter(context.getIntakeAfspraak().getKamer().getIntakelocatie(),
						OrganisatieParameterKey.COLON_INTAKELOCATIE_BESCHRIJVING);
				}
				return null;
			}

		},

	IL_INTAKEDUUR("_IL_INTAKEDUUR")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					var organisatieParameterService = getBean(OrganisatieParameterService.class);
					return organisatieParameterService.getOrganisatieParameter(context.getIntakeAfspraak().getKamer().getIntakelocatie(),
						OrganisatieParameterKey.COLON_DUUR_AFSPRAAK_IN_MINUTEN);
				}

				return null;
			}

		},

	IL_DIGITALE_INTAKE("_IL_DIGITALE_INTAKE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return getOrganisatieParameterService().getOrganisatieParameter(context.getIntakeAfspraak().getKamer().getIntakelocatie(),
						OrganisatieParameterKey.COLON_DIGITALE_INTAKE);
				}
				return null;
			}
		},

	COLON_HERAANMELDEN_TEKST(
		"_COLON_HERAANMELDEN_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getBrief() != null && context.getBrief().getBevolkingsonderzoek() == Bevolkingsonderzoek.COLON)
				{
					return getValueVanHeraanmeldenTekstKey((ColonBrief) context.getBrief());
				}
				if (context.getColonUitnodiging() != null && context.getColonUitnodiging().getScreeningRonde() != null)
				{
					return getValueTekstKeyAlsHeraangemeldeUitnodigingVoorInpakcentrum(context.getColonUitnodiging().getScreeningRonde().getUitnodigingen());
				}
				return null;
			}

		},

	COLON_NIEUWE_FIT_TEKST(
		"_COLON_NIEUWE_FIT_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getColonUitnodiging() != null)
				{
					switch (context.getColonUitnodiging().getColonUitnodigingCategorie())
					{
					case U4:
						return getStringValueFromPreference(PreferenceKey.COLON_NIEUWE_FIT_AANGEVRAAGD_TEKST);
					case U4_2:
						return getStringValueFromPreference(PreferenceKey.COLON_NIEUWE_FIT_NA_HERAANMELDING_TEKST);
					default:
						return null;
					}
				}
				return null;
			}

		},

	COLON_JAAR_VOLGENDE_RONDE(
		"_COLON_JAAR_VOLGENDE_RONDE",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() != null && context.getClient().getColonDossier().getVolgendeUitnodiging() != null)
				{
					var dossierService = getBean(ColonDossierBaseService.class);
					var datumVolgendeUitnodiging = dossierService.getDatumVolgendeUitnodiging(context.getClient().getColonDossier());

					if (datumVolgendeUitnodiging != null)
					{
						return datumVolgendeUitnodiging.getYear();
					}
				}
				return null;
			}

		},

	COLON_LAATSTE_RONDE_TEKST(
		"_COLON_LAATSTE_RONDE_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var maximaleLeeftijd = getSimplePreferenceService().getInteger(PreferenceKey.MAXIMALE_LEEFTIJD_COLON.name());
				var volgendeUitnodigingsDatum = getBean(ColonDossierBaseService.class).getDatumVolgendeUitnodiging(context.getClient().getColonDossier());

				if (volgendeUitnodigingsDatum != null)
				{
					if (!volgendeUitnodigingsDatum.isBefore(DateUtil.toLocalDate(context.getClient().getPersoon().getGeboortedatum()).plusYears(maximaleLeeftijd + 1L)))
					{
						return getSimplePreferenceService().getString(PreferenceKey.COLON_LAATSTE_RONDE_BRIEF_TEKST.name());
					}
					else
					{
						return getSimplePreferenceService().getString(PreferenceKey.COLON_VOLGENDE_RONDE_BRIEF_TEKST.name());
					}
				}
				return null;
			}
		},
	COLON_GEEN_CAPACITEIT_TERMIJN("_COLON_GEEN_CAPACITEIT_TERMIJN")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return getBean(ColonIntakelocatieService.class).getSignaleringstermijnTekst();
			}
		},

	ZI_ADRES("_ZI_ADRES", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return AdresUtil.getVolledigeAdresString(getAdres(zorginstelling, 0));
				}
				return null;
			}
		},

	ZI_NAAM("_ZI_NAAM", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.naam", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return zorginstelling.getNaam();
				}
				return null;
			}
		},

	ZI_STRAATNAAM("_ZI_STRAATNAAM", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[0].straat", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 0);
				if (adres != null)
				{
					return adres.getStraat();
				}
				return null;
			}
		},

	ZI_HUISNUMMER_TOEV("_ZI_HUISNUMMER_TOEVOEGING")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 0);
				if (adres != null)
				{
					return AdresUtil.getHuisnummerVolledig(adres);
				}
				return null;
			}
		},

	ZI_POSTCODE("_ZI_POSTCODE", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[0].postcode", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 0);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}
		},

	ZI_PLAATS("_ZI_PLAATS", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[0].plaats", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 0);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}
		},

	ZI_POSTBUSNR("_ZI_POSTBUSNR", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[1].huisnummer", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 1);
				if (adres != null)
				{
					return adres.getHuisnummer();
				}
				return null;
			}
		},

	ZI_POSTBUSPOSTCODE("_ZI_POSTBUSPOSTCODE", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[1].postcode", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 1);
				if (adres != null)
				{
					return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
				}
				return null;
			}

		},

	ZI_POSTBUSPLAATS("_ZI_POSTBUSPLAATS", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.adressen[1].plaats", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var adres = getAdres(getZorgInstelling(context), 1);
				if (adres != null)
				{
					return adres.getPlaats();
				}
				return null;
			}

		},

	ZI_TEL("_ZI_TEL", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.telefoon", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return zorginstelling.getTelefoon();
				}
				return null;
			}

		},

	ZI_FAX("_ZI_FAX", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.fax", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return zorginstelling.getFax();
				}
				return null;
			}

		},

	ZI_EMAILADRES("_ZI_EMAILADRES", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.email", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return zorginstelling.getEmail();
				}
				return null;
			}

		},

	ZI_WEBSITEADRES("_ZI_WEBSITEADRES", MergeFieldTestType.ZORGINSTELLING, "overeenkomst.gebruiker.organisatieMedewerkers[0].instelling.website", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var zorginstelling = getZorgInstelling(context);
				if (zorginstelling != null)
				{
					return zorginstelling.getWebsite();
				}
				return null;
			}
		},

	ZI_AFDELING("_ZI_AFDELING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return null;
			}
		},

	HA_AANHEF("_HA_AANHEF", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Geachte heer of mevrouw Dokter", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts arts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (arts != null && arts.getOrganisatieMedewerkers() != null && !arts.getOrganisatieMedewerkers().isEmpty())
				{
					var aanhef = "Geachte ";
					var gebruiker = arts.getOrganisatieMedewerkers().get(0).getMedewerker();
					if (gebruiker != null && gebruiker.getAanhef() != null)
					{
						if (gebruiker.getAanhef() == Aanhef.DHR)
						{
							aanhef += "heer";
						}
						else if (gebruiker.getAanhef() == Aanhef.MEVR)
						{
							aanhef += "mevrouw";
						}
						aanhef += " ";
					}
					else
					{
						aanhef += "heer of mevrouw ";
					}
					if (gebruiker != null && gebruiker.getTussenvoegsel() != null)
					{
						aanhef += StringUtils.capitalize(gebruiker.getTussenvoegsel()) + " ";
					}
					if (gebruiker != null && gebruiker.getAchternaam() != null)
					{
						aanhef += StringUtils.capitalize(gebruiker.getAchternaam());
					}

					return aanhef;
				}
				return null;
			}

		},

	HA_NAAM("_HA_NAAM", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Dokter", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts arts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (arts != null && arts.getOrganisatieMedewerkers() != null && !arts.getOrganisatieMedewerkers().isEmpty())
				{
					String naam = null;
					var gebruiker = arts.getOrganisatieMedewerkers().get(0).getMedewerker();
					if (gebruiker != null)
					{
						naam = NaamUtil.getNaamGebruiker(gebruiker);
					}

					return naam;
				}
				return null;
			}

		},

	HA_LOCATIE_ID("_HA_LOCATIE_ID", MergeFieldTestType.BMHKHUISARTS, Integer.class, () -> "123456789", NIET_NAAR_INPAKCENTRUM, QR_CODE)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					return locatie.getId();
				}
				return null;
			}

		},

	HA_LOCATIE_NAAM("_HA_LOCATIE_NAAM", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Praktijk van dokter Arts", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					return locatie.getNaam();
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_KIX(
		"_HA_LOCATIE_ADRES_KIX",
		RoyalMailCBCBean.class,
		null,
		MergeFieldTestType.BMHKHUISARTS,
		String.class,
		() -> "8888XXX8888",
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					return AdresUtil.createKixCode(locatie.getLocatieAdres());
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_VOLLEDIG(
		"_HA_LOCATIE_ADRES_VOLLEDIG",
		MergeFieldTestType.BMHKHUISARTS,
		String.class,
		() -> "Teststraat 123 B, 8888XX Teststad-Utrecht",
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					Adres adres = locatie.getLocatieAdres();
					if (adres != null)
					{
						return AdresUtil.getVolledigeAdresString(adres);
					}
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_STRAATNAAM("_HA_LOCATIE_ADRES_STRAATNAAM", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Teststraat", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					Adres adres = locatie.getLocatieAdres();
					if (adres != null)
					{
						return adres.getStraat();
					}
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_HUISNUMMER_TOEV("_HA_LOCATIE_ADRES_HUISNUMMER_TOEV", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "123 B", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					Adres adres = locatie.getLocatieAdres();
					if (adres != null)
					{
						var adresString = new StringBuilder();
						if (adres.getHuisnummer() != null)
						{
							adresString.append(adres.getHuisnummer());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisletter()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisletter());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisnummerToevoeging()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisnummerToevoeging());
						}

						if (adres.getHuisnummer() == null && !Strings.isNullOrEmpty(adres.getHuisnummerAanduiding()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisnummerAanduiding());
						}

						return adresString.toString();
					}
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_POSTCODE("_HA_LOCATIE_ADRES_POSTCODE", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "8888 XX", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					Adres adres = locatie.getLocatieAdres();
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}

		},

	HA_LOCATIE_ADRES_PLAATS("_HA_LOCATIE_ADRES_PLAATS", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Teststad-Utrecht", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie locatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (locatie != null)
				{
					var adres = locatie.getLocatieAdres();
					if (adres != null && adres.getWoonplaats() != null)
					{
						return adres.getWoonplaats().getNaam();
					}
				}
				return null;
			}

		},

	HA_POST_ADRES_KIX("_HA_POST_ADRES_KIX", RoyalMailCBCBean.class, null, MergeFieldTestType.BMHKHUISARTS, String.class, () -> "8888XXX8888", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts arts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (arts != null)
				{
					return AdresUtil.createKixCode(arts.getPostadres());
				}
				return null;
			}

		},

	HA_POST_ADRES_VOLLEDIG(
		"_HA_POST_ADRES_VOLLEDIG",
		MergeFieldTestType.BMHKHUISARTS,
		String.class,
		() -> "Teststraat 123 B, 8888 XX Teststad-Utrecht",
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (cervixHuisarts != null)
				{
					Adres adres = cervixHuisarts.getPostadres();
					if (adres != null)
					{
						return AdresUtil.getVolledigeAdresString(adres);
					}
				}
				return null;
			}

		},

	HA_POST_ADRES_STRAATNAAM("_HA_POST_ADRES_STRAATNAAM", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Teststraat", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (cervixHuisarts != null)
				{
					Adres adres = cervixHuisarts.getPostadres();
					if (adres != null)
					{
						return adres.getStraat();
					}
				}
				return null;
			}

		},

	HA_POST_ADRES_HUISNUMMER_TOEV("_HA_POST_ADRES_HUISNUMMER_TOEV", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "123 B", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (cervixHuisarts != null)
				{
					Adres adres = cervixHuisarts.getPostadres();
					if (adres != null)
					{
						var adresString = new StringBuilder();
						if (adres.getHuisnummer() != null)
						{
							adresString.append(adres.getHuisnummer());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisletter()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisletter());
						}

						if (!Strings.isNullOrEmpty(adres.getHuisnummerToevoeging()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisnummerToevoeging());
						}

						if (adres.getHuisnummer() == null && !Strings.isNullOrEmpty(adres.getHuisnummerAanduiding()))
						{
							adresString.append(" ");
							adresString.append(adres.getHuisnummerAanduiding());
						}

						return adresString.toString();
					}
				}
				return null;
			}

		},

	HA_POST_ADRES_POSTCODE("_HA_POST_ADRES_POSTCODE", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "8888 XX", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (cervixHuisarts != null)
				{
					Adres adres = cervixHuisarts.getPostadres();
					if (adres != null)
					{
						return PostcodeFormatter.formatPostcode(adres.getPostcode(), true);
					}
				}
				return null;
			}

		},

	HA_POST_ADRES_PLAATS("_HA_POST_ADRES_PLAATS", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Teststad-Utrecht", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				if (cervixHuisarts != null)
				{
					var adres = cervixHuisarts.getPostadres();
					if (adres != null && adres.getWoonplaats() != null)
					{
						return adres.getWoonplaats().getNaam();
					}
				}
				return null;
			}

		},

	HA_REGISTRATIE_CODE("_HA_REGISTRATIE_CODE", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "Wxr-j3O-uKl", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				Instelling uitstrijkendArts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				String registratieCode = null;
				if (uitstrijkendArts != null)
				{
					registratieCode = uitstrijkendArts.getOrganisatieMedewerkers().get(0).getMedewerker().getWachtwoordChangeCode();
				}
				return registratieCode;
			}

		},

	HA_LAB_FORM_VOLGNUMMER("_HA_LAB_FORM_VOLGNUMMER", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "99", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return context.<Integer> getValue(MailMergeContext.CONTEXT_HA_LAB_FORM_VOLGNUMMER);
			}

		},

	HA_AANTAL_FORM("_HA_AANTAL_FORM", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "99", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return context.<Integer> getValue(MailMergeContext.CONTEXT_HA_AANTAL_FORM);
			}

		},

	HA_AGB_CODE("_HA_AGB_CODE", MergeFieldTestType.BMHKHUISARTS, String.class, () -> "01012345", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				Instelling uitstrijkendArts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
				String agbcode = null;
				if (uitstrijkendArts != null)
				{
					agbcode = uitstrijkendArts.getAgbcode();
				}
				return agbcode;
			}

		},

	DATUM_VANDAAG("_DATUM_VANDAAG", MergeFieldTestType.OVERIGE, Date.class, () -> "31-12-2000")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return getFormattedDateZonderDagnaam(new Date());
			}

		},

	DATUM_INTAKE("_DATUM_INTAKE", MergeFieldTestType.INTAKE, Date.class, () -> "01-12-2017")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return getFormattedDateMetDagnaam(context.getIntakeAfspraak().getVanaf());
				}
				return null;
			}

		},

	TIJDSTIP_INTAKE("_TIJDSTIP_INTAKE", MergeFieldTestType.INTAKE, String.class, () -> "08:45")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getIntakeAfspraak() != null)
				{
					return DateUtil.formatLocalTime(context.getIntakeAfspraak().getVanaf());
				}
				return null;
			}

		},

	DATUM_VORIGE_INTAKE("_DATUM_VORIGEINTAKE", MergeFieldTestType.INTAKE, Date.class, () -> "29-05-2017")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getVorigeIntakeAfspraak() != null)
				{
					return getFormattedDateMetDagnaam(context.getVorigeIntakeAfspraak().getVanaf());
				}

				return null;
			}

		},

	TIJDSTIP_VORIGE_INTAKE("_TIJDSTIP_VORIGEINTAKE", MergeFieldTestType.INTAKE, String.class, () -> "10:30")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getVorigeIntakeAfspraak() != null)
				{
					return DateUtil.formatLocalTime(context.getVorigeIntakeAfspraak().getVanaf());
				}
				return null;
			}

		},

	DATUM_VORIGECOLOSCOPIE("_DATUM_VORIGECOLOSCOPIE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return null;
			}

		},

	TIJDSTIP_VORIGECOLOSCOPIE("_TIJDSTIP_VORIGECOLOSCOPIE")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return null;
			}

		},

	UITNODIGINGSID("_UITNODIGINGSID", UitnodigingIdBarcode.class, null, MergeFieldTestType.OVERIGE, "cervixUitnodiging.uitnodigingsId", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				InpakbareUitnodiging<?> inpakbareUitnodiging = null;
				if (context.getColonUitnodiging() != null)
				{
					inpakbareUitnodiging = context.getColonUitnodiging();
				}
				else if (context.getCervixUitnodiging() != null)
				{
					inpakbareUitnodiging = context.getCervixUitnodiging();
				}
				if (inpakbareUitnodiging != null)
				{
					return inpakbareUitnodiging.getUitnodigingsId().toString();
				}
				return null;
			}

		},

	MAMMA_UITNODIGINGSNUMMER("_BK_UITNODIGINGSNUMMER", UitnodigingIdBarcode.class, null, MergeFieldTestType.OVERIGE, String.class, () -> "123456789")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				return getMammaUitnodigingsNummer(context);
			}

		},
	MAMMA_UITNODIGINGSNUMMER_EMAIL_BARCODE("_BK_UITNODIGINGSNUMMER_EMAIL_BARCODE", UitnodigingIdBarcode.class, null, MergeFieldTestType.OVERIGE, String.class,
		() -> "<img src='data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAMYAAADRAQAAAAC/M/DTAAAACXBIWXMAAC4jAAAuIwF4pT92AAAAEnRFWHRTb2Z0d2FyZQBCYXJjb2RlNEryjnYuAAAAvUlEQVR4Xu3QMQ6CQBQE0G8s6OQIexO5knRUgrGg5EoYC6+BN8Buiw0jW7jyk68JnSYzBcnwks3sCoCwK+SYDfO39O4EKfv4UygUCoVCoVAoFAqFQqFQfkjM/Iv4HI0tw3aSwpR+E8SZch78Pjel8xVaU1wocVspCPV68fWHBfDzPlse84pUlNwxuVSUXBGKVJS0ccMrSnKM77KU4HCxRV1HyRjPM0WNVqJGK+ni86QspZFKMlNEDqtFh/JVnk4BQA0jieC8AAAAAElFTkSuQmCC' alt='barcode 22'>")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodigingsNummer = getMammaUitnodigingsNummer(context);
				if (uitnodigingsNummer != null)
				{
					var barcodeBean = new UitnodigingIdBarcode();
					var barcodeService = getBean(BarcodeService.class);
					try (var barcodeInputStream = barcodeService.maakBarcodeInputStreamVoorEmail(uitnodigingsNummer, barcodeBean))
					{
						var bytes = IOUtils.toByteArray(barcodeInputStream);
						var barcodeBase64String = Base64.getEncoder().encodeToString(bytes);
						return String.format("<img src='data:image/png;base64, %s' alt='barcode %s'/>", barcodeBase64String, uitnodigingsNummer);
					}
					catch (IOException e)
					{

						LOG.warn("Fout bij maken e-mail barcode voor BK uitnodigingsnummer: {}", uitnodigingsNummer, e);
					}
				}
				return null;
			}
		},

	ZV_ACHTERNAAM("_ZV_ACHTERNAAM", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.achternaam", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getAchternaam();
				}
				return null;
			}

		},

	ZV_TUSSENVOEGSEL("_ZV_TUSSENVOEGSEL", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.tussenvoegsel", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getTussenvoegsel();
				}
				return null;
			}

		},

	ZV_VOORLETTERS("_ZV_VOORLETTERS", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.voorletters", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getVoorletters();
				}
				return null;
			}

		},

	ZV_AANHEF("_ZV_AANHEF", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.aanhef", Aanhef.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst
					&& ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getAanhef() != null)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getAanhef().getNaam();
				}
				return null;
			}

		},

	ZV_TITEL("_ZV_TITEL", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.titel", Titel.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst
					&& ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getTitel() != null)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getTitel().getNaam();
				}
				return null;
			}

		},

	ZV_FUNCTIE("_ZV_FUNCTIE", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.functie", Functie.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst
					&& ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getFunctie() != null)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getFunctie().getNaam();
				}
				return null;
			}

		},

	ZV_UZINR("_ZV_UZINR", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.uzinummer", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getUzinummer();

				}
				return null;
			}

		},

	ZV_PLAATS("_ZV_PLAATS", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.adressen[0].plaats", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var overeenkomst = context.getOvereenkomst();
				if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					var afgeslotenKwaliteitsOvereenkomst = (AfgeslotenMedewerkerOvereenkomst) overeenkomst;
					var adres = getAdres(afgeslotenKwaliteitsOvereenkomst.getGebruiker());
					if (adres != null)
					{
						return adres.getPlaats();
					}

				}
				return null;
			}

		},

	ZV_GEBOORTEDATUM("_ZV_GEBOORTEDATUM", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.geboortedatum", Date.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return new SimpleDateFormat("dd-MM-yyyy").format(((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getGeboortedatum());
				}
				return null;
			}

		},

	ZV_BIGNR("_ZV_BIGNR", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.bignummer", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getBignummer();

				}
				return null;
			}

		},

	ZV_EMAIL("_ZV_EMAIL", MergeFieldTestType.ZORGVERLENER, "overeenkomst.gebruiker.emailextra", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenMedewerkerOvereenkomst)
				{
					return ((AfgeslotenMedewerkerOvereenkomst) context.getOvereenkomst()).getGebruiker().getEmailextra();
				}
				return null;
			}

		},

	SO_VERTEGENWOORDIGER("_SO_VERTEGENWOORDIGER")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var screeningOrganisatie = getScreeningOrganisatie(context);
				if (screeningOrganisatie != null)
				{
					return screeningOrganisatie.getVertegenwoordiger();
				}
				return null;
			}

		},

	ZI_GEM_ACHTERNAAM("_ZI_GEM_ACHTERNAAM", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getAchternaam();
					}
				}
				return null;
			}

		},

	ZI_GEM_TUSSENVOEGSEL("_ZI_GEM_TUSSENVOEGSEL", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getTussenvoegsel();
					}
				}
				return null;
			}

		},

	ZI_GEM_VOORLETTERS("_ZI_GEM_VOORLETTERS")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getVoorletters();
					}
				}
				return null;
			}

		},

	ZI_GEM_AANHEF("_ZI_GEM_AANHEF", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null && afgeslotenOvereenkomst.getInstelling().getGemachtigde().getAanhef() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getAanhef().getNaam();
					}
				}
				return null;
			}

		},

	ZI_GEM_TITEL("_ZI_GEM_TITEL", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null && afgeslotenOvereenkomst.getInstelling().getGemachtigde().getTitel() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getTitel().getNaam();
					}
				}
				return null;
			}

		},

	ZI_GEM_FUNCTIE("_ZI_GEM_FUNCTIE", MergeFieldTestType.ZORGINSTELLING, String.class, () -> "xxx")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() instanceof AfgeslotenInstellingOvereenkomst)
				{
					var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
					if (afgeslotenOvereenkomst.getInstelling().getGemachtigde() != null && afgeslotenOvereenkomst.getInstelling().getGemachtigde().getFunctie() != null)
					{
						return afgeslotenOvereenkomst.getInstelling().getGemachtigde().getFunctie().getNaam();
					}
				}
				return null;
			}

		},

	OV_INGANGSDATUM("_OV_INGANGSDATUM", MergeFieldTestType.OVERIGE, "overeenkomst.startDatum", Date.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getOvereenkomst() != null)
				{
					return getFormattedDateMetDagnaam(DateUtil.toLocalDateTime(context.getOvereenkomst().getStartDatum()));
				}
				return null;
			}

		},

	CERVIX_MONSTER_ID("_CERVIX_MONSTER_ID", CervixMonsterIdBarcode.class, null)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = context.getCervixUitnodiging();
				if (uitnodiging != null)
				{
					setBarcodeTypeFromContext(context);
					return uitnodiging.getMonster().getMonsterId();
				}
				return null;
			}

			private void setBarcodeTypeFromContext(MailMergeContext context)
			{
				if (context.getBrief() == null)
				{
					setBarcodeType(CervixMonsterIdLabelBarcode.class);
				}
				else
				{
					setBarcodeType(CervixMonsterIdBarcode.class);
				}
			}
		},

	CERVIX_MONSTER_CONTROLE_LETTERS(
		"_CERVIX_MONSTER_CONTROLE_LETTERS",
		MergeFieldTestType.OVERIGE,
		"cervixUitnodiging.monster.controleLetters",
		String.class,
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT,
		QR_CODE,
		WAARDE_NIET_TRIMMEN)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = context.getCervixUitnodiging();
				return uitnodiging != null
					&& uitnodiging.getMonsterType().equals(CervixMonsterType.UITSTRIJKJE)
					? ((CervixUitstrijkje) uitnodiging.getMonster()).getControleLetters()
					: null;
			}
		},

	BMHKLAB_NAAM("_BMHKLAB_NAAM", MergeFieldTestType.BMHKLAB, "bmhkLaboratorium.naam", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var cervixUitnodiging = context.getCervixUitnodiging();
				if (cervixUitnodiging != null)
				{
					var monster = cervixUitnodiging.getMonster();
					if (monster != null)
					{
						var laboratorium = monster.getLaboratorium();
						if (laboratorium != null)
						{
							return laboratorium.getNaam();
						}
					}
				}
				else
				{
					String labnaam = null;
					CervixHuisartsLocatie cervixHuisartsLocatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
					if (cervixHuisartsLocatie != null && cervixHuisartsLocatie.getLocatieAdres() != null && cervixHuisartsLocatie.getLocatieAdres().getWoonplaats() != null
						&& cervixHuisartsLocatie.getLocatieAdres().getWoonplaats().getGemeente() != null)
					{
						var laboratorium = cervixHuisartsLocatie.getLocatieAdres().getWoonplaats().getGemeente().getBmhkLaboratorium();
						if (laboratorium != null)
						{
							labnaam = laboratorium.getNaam();
						}
					}
					if (labnaam == null)
					{
						CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
						if (cervixHuisarts != null && cervixHuisarts.getPostadres() != null && cervixHuisarts.getPostadres().getWoonplaats() != null
							&& cervixHuisarts.getPostadres().getWoonplaats().getGemeente() != null)
						{
							var laboratorium = cervixHuisarts.getPostadres().getWoonplaats().getGemeente().getBmhkLaboratorium();
							if (laboratorium != null)
							{
								labnaam = laboratorium.getNaam();
							}
						}
					}
					return labnaam;
				}
				return null;
			}

		},

	BMHKLAB_ONDERTEKENAAR_HA_BERICHT_CYTO(
		"_BMHKLAB_ONDERTEKENAAR_HA_BERICHT_CYTO",
		MergeFieldTestType.BMHKLAB,
		"bmhkLaboratorium.patholoog",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var deproxy = HibernateHelper.deproxy(context.getBrief());
				if (deproxy instanceof CervixBrief)
				{
					var brief = (CervixBrief) deproxy;
					if (CervixMonsterUtil.isUitstrijkje(brief.getMonster()))
					{
						var monster = CervixMonsterUtil.getUitstrijkje(brief.getMonster());
						if (monster.getCytologieVerslag() != null)
						{
							return monster.getCytologieVerslag().getPatholoogNaam();
						}
					}
				}
				return null;
			}
		},

	BMHKLAB_ONDERTEKENAAR("_BMHKLAB_ONDERTEKENAAR", MergeFieldTestType.BMHKLAB, "bmhkLaboratorium.patholoog", String.class)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var deproxy = HibernateHelper.deproxy(context.getBrief());
				if (deproxy instanceof CervixBrief)
				{
					var brief = (CervixBrief) deproxy;
					return getBMHKLaboratoriumOndertekenaar(brief, false);
				}
				if (context.getBmhkLaboratorium() != null)
				{
					return getBMHKLaboratoriumOndertekenaar(context.getBmhkLaboratorium(), false);
				}
				return null;
			}

		},

	BMHKLAB_HANDTEKENING("_BMHKLAB_HANDTEKENING", NIET_IN_HUISARTSBERICHT, NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var deproxy = HibernateHelper.deproxy(context.getBrief());
				if (deproxy instanceof CervixBrief)
				{
					var brief = (CervixBrief) deproxy;
					return getBMHKLaboratoriumOndertekenaar(brief, true);
				}
				if (context.getBmhkLaboratorium() != null)
				{
					return getBMHKLaboratoriumOndertekenaar(context.getBmhkLaboratorium(), true);
				}
				return null;
			}

		},

	CERVIX_CYTO_PV("_CERVIX_CYTO_PV", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = context.getCervixUitnodiging();
				if (uitnodiging != null)
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(uitnodiging.getMonster());
					var verslag = uitstrijkje.getCytologieVerslag();
					if (verslag != null)
					{
						var cytologieUitslagHuisarts = verslag.getVerslagContent().getCytologieUitslagBvoBmhkTbvHuisarts();
						return cytologieUitslagHuisarts.getProtocollairVerslag();
					}
				}
				return null;
			}

		},

	CERVIX_CYTO_CONCLUSIE("_CERVIX_CYTO_CONCLUSIE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = context.getCervixUitnodiging();
				if (uitnodiging != null)
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(uitnodiging.getMonster());
					var verslag = uitstrijkje.getCytologieVerslag();
					if (verslag != null)
					{
						var cytologieUitslagHuisarts = verslag.getVerslagContent().getCytologieUitslagBvoBmhkTbvHuisarts();
						return cytologieUitslagHuisarts.getConclusie();
					}
				}
				return null;
			}

		},

	CERVIX_NIET_ANALYSEERBAAR_REDEN("_CERVIX_NIET_ANALYSEERBAAR_REDEN", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = context.getCervixUitnodiging();
				if (uitnodiging != null)
				{
					var uitstrijkje = CervixMonsterUtil.getUitstrijkje(uitnodiging.getMonster());
					if (uitstrijkje.getUitstrijkjeStatus() == CervixUitstrijkjeStatus.NIET_ANALYSEERBAAR)
					{
						return uitstrijkje.getNietAnalyseerbaarReden().getNaam();
					}
				}
				return CervixNietAnalyseerbaarReden.ONBEKEND.getNaam();
			}

		},

	CERVIX_HERINNERINGSTEKST(
		"_CERVIX_HERINNERINGSTEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = getOorspronkelijkeCervixUitnoding(context.getCervixUitnodiging());
				if (uitnodiging != null && Boolean.TRUE.equals(uitnodiging.getHerinnering()))
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_HERINNERING_TEKST);
				}
				return "";
			}

		},

	CERVIX_HERAANMELDEN_TEKST(
		"_CERVIX_HERAANMELDEN_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getBrief() != null && context.getBrief().getBevolkingsonderzoek() == Bevolkingsonderzoek.CERVIX)
				{
					return getValueVanHeraanmeldenTekstKey((CervixBrief) context.getBrief());
				}
				return null;
			}

		},

	CERVIX_UITGESTELD_TEKST(
		"_CERVIX_UITGESTELD_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var uitnodiging = getOorspronkelijkeCervixUitnoding(context.getCervixUitnodiging());
				if (uitnodiging != null && Boolean.TRUE.equals(uitnodiging.getUitgesteld()))
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_UITGESTELD_TEKST);
				}
				return "";
			}

		},

	CERVIX_NIEUWE_ZAS_AANGEVRAAGD_TEKST(
		"_CERVIX_NIEUWE_ZAS_AANGEVRAAGD_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var redenUitnodiging = Optional.of(context.getCervixUitnodiging()).orElseGet(CervixUitnodiging::new).getRedenUitnodiging();
				if (redenUitnodiging == CervixRedenUitnodiging.NIEUWE_ZAS_NA_OUDE_INGESTUURDE_ZAS)
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_NIEUWE_ZAS_NA_OUDE_INGESTUURDE_ZAS_TEKST);
				}
				else
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_NIEUWE_ZAS_STANDAARD_TEKST);
				}
			}
		},

	CERVIX_VERVOLGONDERZOEK_NEGATIEF(
		"_CERVIX_VERVOLGONDERZOEK_NEGATIEF_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "Er is geen HPV (humaan papillomavirus) gevonden.",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var leeftijdcategorie = getCervixLeeftijdcategorie(context);
				if (CervixLeeftijdcategorie._70.equals(leeftijdcategorie) || CervixLeeftijdcategorie._65.equals(leeftijdcategorie))
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_VERVOLGONDERZOEK_NEGATIEF_65PLUS_TEKST);
				}
				else if (CervixLeeftijdcategorie._60.equals(leeftijdcategorie))
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_VERVOLGONDERZOEK_NEGATIEF_60PLUS_TEKST);
				}
				else
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_VERVOLGONDERZOEK_NEGATIEF_OVERIGE_TEKST);
				}
			}
		},

	CERVIX_CYTOLOGIE_POSITIEF(
		"_CERVIX_CYTOLOGIE_POSITIEF_TEKST",
		MergeFieldTestType.OVERIGE,
		String.class,
		() -> "Er is HPV (humaan papillomavirus) gevonden. Ook zijn er afwijkende cellen gevonden.",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var leeftijdcategorie = getCervixLeeftijdcategorie(context);
				if (CervixLeeftijdcategorie._70.equals(leeftijdcategorie) || CervixLeeftijdcategorie._65.equals(leeftijdcategorie) || CervixLeeftijdcategorie._60.equals(
					leeftijdcategorie))
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_CYTOLOGIE_POSITIEF_60PLUS_TEKST);
				}
				else
				{
					return getStringValueFromPreference(PreferenceKey.CERVIX_CYTOLOGIE_POSITIEF_OVERIGE_TEKST);
				}
			}

		},

	CERVIX_HUISARTS_DOORGEVEN_TOT_DATUM(
		"_CERVIX_HUISARTS_DOORGEVEN_TOT_DATUM",
		MergeFieldTestType.OVERIGE,
		Date.class,
		() -> "31-12-2017",
		NIET_NAAR_INPAKCENTRUM,
		NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getBrief() != null && context.getBrief().getBriefType() == BriefType.CERVIX_HUISARTS_ONBEKEND)
				{
					var brief = (CervixBrief) context.getBrief();
					while (brief.getLabformulier() == null)
					{
						brief = (CervixBrief) BriefUtil.getHerdruk(brief);
					}
					var uitstrijkje = brief.getLabformulier().getUitstrijkje();
					if (uitstrijkje != null)
					{
						var ontvangstdatum = brief.getLabformulier().getUitstrijkje().getOntvangstdatum();
						var wachttijdHuisartsDoorgeven = getSimplePreferenceService().getInteger(PreferenceKey.CERVIX_WACHTTIJD_HUISARTS_DOORGEVEN.toString());
						return DateUtil.formatShortDate(DateUtil.plusWerkdagen(ontvangstdatum, wachttijdHuisartsDoorgeven));
					}
					else
					{
						LOG.warn("Uitstrijkje voor labformulier (id: '{}') kon niet worden gevonden", brief.getLabformulier().getId());
					}
				}
				return null;
			}

		},

	CERVIX_BETAALOPDRACHT_BETALINGSOMSCHRIJVING(
		"_BTO_BETALINGSOMSCHRIJVING",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					return betaalopdracht.getOmschrijving();
				}
				return null;
			}

		},

	CERVIX_BETAALOPDRACHT_BETALINGSKENMERK(
		"_BTO_BETALINGSKENMERK",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					return betaalopdracht.getBetalingskenmerk();
				}
				return null;
			}

		},

	CERVIX_BETAALOPDRACHT_GEHEELTOTAALBEDRAG(
		"_BTO_GEHEELTOTAALBEDRAG",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					var totaalBedrag = getBean(CervixBaseBetalingService.class).totaalBedragInBetaalopdracht(betaalopdracht);
					return NumberFormat.getCurrencyInstance().format(totaalBedrag);
				}
				return null;
			}

		},

	CERVIX_BETAALOPDRACHT_LABS_TOTAALBEDRAG(
		"_BTO_LABS_TOTAALBEDRAG",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					var totaalBedrag = getBean(CervixBaseBetalingService.class).totaalBedragLaboratoriumBoekRegelsInBetaalopdracht(betaalopdracht);
					return NumberFormat.getCurrencyInstance().format(totaalBedrag);
				}
				return null;
			}

		},

	CERVIX_BETAALOPDRACHT_HA_AANTAL_UITSTRIJKJES(
		"_BTO_HA_AANTAL_UITSTRIJKJES",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					return getBean(CervixBaseBetalingService.class).totaalAantalHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, false);
				}
				return null;
			}

		},
	CERVIX_BETAALOPDRACHT_HA_TOTAALBEDRAG_UITSTRIJKJES(
		"_BTO_HA_TOTAALBEDRAG_UITSTRIJKJES",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					var totaalBedrag = getBean(CervixBaseBetalingService.class).totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, false);
					return NumberFormat.getCurrencyInstance().format(totaalBedrag);
				}
				return null;
			}
		},
	CERVIX_BETAALOPDRACHT_HA_TOTAALBEDRAG_CORRECTIES(
		"_BTO_HA_TOTAALBEDRAG_CORRECTIES",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					var totaalBedrag = getBean(CervixBaseBetalingService.class).totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, true);
					return NumberFormat.getCurrencyInstance().format(totaalBedrag);
				}
				return null;
			}
		},
	CERVIX_BETAALOPDRACHT_HA_AANTAL_CORRECTIES(
		"_BTO_HA_AANTAL_CORRECTIES",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					return getBean(CervixBaseBetalingService.class).totaalAantalHuisartsBoekRegelsInBetaalopdracht(betaalopdracht, true) * -1;
				}
				return null;
			}
		},
	CERVIX_BETAALOPDRACHT_HA_TOTAALBEDRAG(
		"_BTO_HA_TOTAALBEDRAG",
		NIET_IN_HUISARTSBERICHT,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var betaalopdracht = MergeField.getBetaalopdracht(context);
				if (betaalopdracht != null)
				{
					var totaalBedrag = getBean(CervixBaseBetalingService.class).totaalBedragHuisartsBoekRegelsInBetaalopdracht(betaalopdracht);
					return NumberFormat.getCurrencyInstance().format(totaalBedrag);
				}
				return null;
			}
		},

	CERVIX_HUISARTS_ZORGMAIL_VERIFICATIE_PINCODE(
		"_CERVIX_HUISARTS_ZORGMAIL_VERIFICATIE_PINCODE",
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				CervixHuisartsLocatie huisartsLocatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
				if (huisartsLocatie != null)
				{
					return huisartsLocatie.getVerificatieCode();
				}
				return "";
			}
		},

	MAMMA_AFSPRAAK_DATUM("_BK_AFSPRAAK_DATUM", MergeFieldTestType.BKAFSPRAAK, Date.class, () -> "01-12-2017", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var afspraak = getLaatsteMammaAfspraak(context);
				if (afspraak != null)
				{
					return getFormattedDateMetDagnaam(afspraak.getVanaf());
				}
				return null;
			}
		},

	MAMMA_AFSPRAAK_TIJD("_BK_AFSPRAAK_TIJD", MergeFieldTestType.BKAFSPRAAK, String.class, () -> "08:45", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var afspraak = getLaatsteMammaAfspraak(context);
				if (afspraak != null)
				{
					return DateUtil.toLocalTime(afspraak.getVanaf()).format(DateTimeFormatter.ofPattern("HH:mm"));
				}
				return null;
			}
		},

	MAMMA_AFSPRAAK_REDEN_VERZET("_BK_AFSPRAAK_REDEN_VERZET",
		MergeFieldTestType.BKAFSPRAAK, MammaVerzettenReden.class, () -> null,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var laatsteAfspraak = getLaatsteMammaAfspraak(context);
				if (laatsteAfspraak != null && MammaVerzettenReden.briefVerplicht(laatsteAfspraak.getVerzettenReden()))
				{
					var vorigeAfspraak = getEenNaLaatsteAfspraak(laatsteAfspraak.getUitnodiging());

					if (vorigeAfspraak.getStatus().equals(MammaAfspraakStatus.GEPLAND))
					{
						return getStringValueFromPreference(PreferenceKey.MAMMA_BULK_VERZETTEN_VERLEDEN_AFSPRAAK_TEKST);
					}
					else
					{
						return getStringValueFromPreference(PreferenceKey.MAMMA_BULK_VERZETTEN_TOEKOMST_AFSPRAAK_TEKST);
					}
				}
				return "";
			}
		},

	MAMMA_AFSPRAAK_LOCATIE_WIJZIGING("_BK_AFSPRAAK_LOCATIE_WIJZIGING",
		MergeFieldTestType.BKAFSPRAAK, String.class, () -> getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_LOCATIE_WIJZIGING_TEKST),
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var toonLocatieWijzigingTekst = Boolean.TRUE.equals(context.getValue(MailMergeContext.CONTEXT_MAMMA_TOON_LOCATIE_WIJZIGING_TEKST));
				if (toonLocatieWijzigingTekst)
				{
					return getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_LOCATIE_WIJZIGING_TEKST);
				}
				var laatsteAfspraak = getLaatsteMammaAfspraak(context);
				if (laatsteAfspraak != null)
				{
					var vorigeAfspraak = getEenNaLaatsteAfspraak(laatsteAfspraak.getUitnodiging());
					if (vorigeAfspraak != null)
					{
						var vorigeStandplaatsLocatie = vorigeAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getLocatie();
						var standplaatsLocatie = laatsteAfspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats().getLocatie();
						if (!AdresUtil.isZelfdeStandplaatsLocatie(vorigeStandplaatsLocatie, standplaatsLocatie))
						{
							return getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_LOCATIE_WIJZIGING_TEKST);
						}
					}
				}

				return null;
			}
		},

	MAMMA_AFSPRAAK_BETREFT(
		"_BK_AFSPRAAK_BETREFT",
		MergeFieldTestType.BKAFSPRAAK,
		null,
		() -> getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_BETREFT_BEVESTIGING_TEKST),
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var laatsteAfspraak = getLaatsteMammaAfspraak(context);
				if (laatsteAfspraak != null)
				{
					List<MammaAfspraak> afspraken = new ArrayList<>(laatsteAfspraak.getUitnodiging().getAfspraken());
					afspraken.sort(Comparator.comparing(MammaAfspraak::getCreatiedatum));

					if (afspraken.size() > 1 && MammaAfspraakStatus.VERPLAATST.equals(afspraken.get(afspraken.size() - 2).getStatus()))
					{
						return getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_BETREFT_WIJZIGING_TEKST);
					}
					else
					{
						return getStringValueFromPreference(PreferenceKey.MAMMA_AFSPRAAK_BETREFT_BEVESTIGING_TEKST);
					}

				}
				return null;
			}
		},

	MAMMA_SP_STRAATNAAM(
		"_BK_SP_STRAATNAAM",
		MergeFieldTestType.BKSTANDPLAATS,
		"client.mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.standplaatsPeriode.standplaatsRonde.standplaats.locatie.straat",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var locatie = getMammaStandplaatsLocatieAfspraak(context);
				if (locatie != null)
				{
					return locatie.getStraat();
				}
				return null;
			}
		},

	MAMMA_SP_HUISNUMMER(
		"_BK_SP_HUISNUMMER",
		MergeFieldTestType.BKSTANDPLAATS,
		"client.mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.standplaatsPeriode.standplaatsRonde.standplaats.locatie.huisnummer",
		String.class,
		NIET_NAAR_INPAKCENTRUM,
		WAARDE_NIET_TRIMMEN)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var locatie = getMammaStandplaatsLocatieAfspraak(context);
				if (locatie != null && locatie.getToonHuisnummerInBrieven())
				{
					var huisnummerString = " " + locatie.getHuisnummer();
					if (locatie.getHuisnummerToevoeging() != null)
					{
						huisnummerString += " " + locatie.getHuisnummerToevoeging();
					}
					return huisnummerString;
				}
				return null;
			}

		},

	MAMMA_SP_POSTCODE(
		"_BK_SP_POSTCODE",
		MergeFieldTestType.BKSTANDPLAATS,
		"client.mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.standplaatsPeriode.standplaatsRonde.standplaats.locatie.postcode",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var locatie = getMammaStandplaatsLocatieAfspraak(context);
				if (locatie != null)
				{
					return PostcodeFormatter.formatPostcode(locatie.getPostcode(), true);
				}
				return null;
			}
		},

	MAMMA_SP_PLAATS(
		"_BK_SP_PLAATS",
		MergeFieldTestType.BKSTANDPLAATS,
		"client.mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.standplaatsPeriode.standplaatsRonde.standplaats.locatie.plaats",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var locatie = getMammaStandplaatsLocatieAfspraak(context);
				if (locatie != null)
				{
					return locatie.getPlaats();
				}
				return null;
			}
		},

	MAMMA_SP_LOC_OMSCHRIJVING(
		"_BK_SP_LOC_OMSCHRIJVING",
		MergeFieldTestType.BKSTANDPLAATS,
		"client.mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.standplaatsPeriode.standplaatsRonde.standplaats.locatie.locatieBeschrijving",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var locatie = getMammaStandplaatsLocatieAfspraak(context);
				if (locatie != null)
				{
					return locatie.getLocatieBeschrijving();
				}
				return null;
			}
		},

	MAMMA_ONDERZOEK_DATUM("_BK_ONDERZOEK_DATUM", MergeFieldTestType.BKAFSPRAAK, Date.class, () -> "10-10-2018", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var beoordeling = getMammaBeoordeling(context);
				Date onderzoekDatum = null;
				if (beoordeling != null && beoordeling.getOnderzoek() != null && beoordeling.getOnderzoek().getCreatieDatum() != null)
				{
					onderzoekDatum = beoordeling.getOnderzoek().getCreatieDatum();
				}
				else
				{
					var afspraak = getAfspraakVanLaatsteOnderzoek(context);
					if (afspraak != null)
					{
						onderzoekDatum = afspraak.getOnderzoek().getCreatieDatum();
					}
				}
				if (onderzoekDatum != null)
				{
					return Constants.getDateFormat().format(onderzoekDatum);
				}
				return null;
			}
		},
	MAMMA_ONDERZOEK_1_VER_RADIOLOOG_ONDERTEKENAAR(
		"_BK_ONDERZ_1_VER_RADIOLOOG_ONDERTEKENAAR",
		MergeFieldTestType.BKRADIOLOOG,
		"radioloog1",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var radioloog = getMammaRadioloog1(context);
				if (radioloog != null)
				{
					return radioloog.getOndertekenaar();
				}
				return null;
			}

		},
	MAMMA_ONDERZOEK_1_VER_RADIOLOOG_HANDTEKENING("_BK_ONDERZ_1_VER_RADIOLOOG_HANDTEKENING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var radioloog = getMammaRadioloog1(context);
				if (radioloog != null)
				{
					return radioloog.getHandtekening();
				}
				return null;
			}
		},
	MAMMA_ONDERZOEK_2_VER_RADIOLOOG_ONDERTEKENAAR(
		"_BK_ONDERZ_2_VER_RADIOLOOG_ONDERTEKENAAR",
		MergeFieldTestType.BKRADIOLOOG,
		"radioloog2",
		String.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var radioloog = getMammaRadioloog2(context);
				if (radioloog != null)
				{
					return radioloog.getOndertekenaar();
				}
				return null;
			}
		},
	MAMMA_ONDERZOEK_2_VER_RADIOLOOG_HANDTEKENING("_BK_ONDERZ_2_VER_RADIOLOOG_HANDTEKENING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var radioloog = getMammaRadioloog2(context);
				if (radioloog != null)
				{
					return radioloog.getHandtekening();
				}
				return null;
			}
		},

	MAMMA_ONDERBROKEN_ONDERZOEKEN_BEELDEN_TEKST(
		"_BK_ONDERBROKEN_ONDERZOEK_BEELDEN_TEKST",
		MergeFieldTestType.BKAFSPRAAK,
		String.class,
		() -> "Tekst over onderbroken onderzoeken met resp. zonder beelden",
		NIET_NAAR_INPAKCENTRUM,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var afspraak = getAfspraakVanLaatsteOnderzoek(context);
				if (afspraak != null)
				{
					var onderzoek = afspraak.getOnderzoek();
					if (onderzoek != null && onderzoek.getMammografie() != null && onderzoek.getStatus() == MammaOnderzoekStatus.ONDERBROKEN)
					{

						if (MammaMammografieIlmStatus.beeldenBeschikbaarOfBeschikbaarGeweest(onderzoek.getMammografie().getIlmStatus()))
						{
							return getStringValueFromPreference(PreferenceKey.MAMMA_ONDERBROKEN_ONDERZOEK_MET_BEELDEN_TEKST);
						}
						else
						{
							return getStringValueFromPreference(PreferenceKey.MAMMA_ONDERBROKEN_ONDERZOEK_ZONDER_BEELDEN_TEKST);
						}
					}
				}
				return null;
			}
		},

	MAMMA_UITSLAG_BIRADS_LINKS("_BK_UITSLAG_BIRADS_LINKS", MergeFieldTestType.BKAFSPRAAK, Integer.class, () -> "1", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var verslaglezing = getVerslagLezing(context);
				return bepaalVerslagBiradsTekst(verslaglezing, MammaZijde.LINKER_BORST);
			}
		},

	MAMMA_UITSLAG_BIRADS_RECHTS("_BK_UITSLAG_BIRADS_RECHTS", MergeFieldTestType.BKAFSPRAAK, Integer.class, () -> "1", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var verslaglezing = getVerslagLezing(context);
				return bepaalVerslagBiradsTekst(verslaglezing, MammaZijde.RECHTER_BORST);
			}
		},

	MAMMA_OPMERKING_VERSLAG("_BK_OPMERKING_VERSLAG", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var verslaglezing = getVerslagLezing(context);
				return verslaglezing != null && verslaglezing.getBiradsOpmerking() != null ? "Opmerking:" + System.lineSeparator() + verslaglezing.getBiradsOpmerking() : null;
			}
		},

	MAMMA_HUISARTSBERICHT_LAESIES("_BK_HUISARTSBERICHT_LAESIES", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var verslaglezing = getVerslagLezing(context);
				return createLaesiesTekst(verslaglezing);
			}
		},

	MAMMA_NEVENBEVINDINGEN("_BK_ONDERZOEK_NEVENBEVINDINGEN", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var beoordeling = getMammaBeoordelingMetEersteEnOfTweedeLezing(context);
				if (beoordeling != null && (!beoordeling.getEersteLezing().getNevenbevindingen().isEmpty() || !beoordeling.getTweedeLezing().getNevenbevindingen().isEmpty()))
				{
					return getNevenbevindingenEnumString(beoordeling);
				}
				return null;
			}
		},

	MAMMA_NEVENBEVINDINGEN_OPMERKINGEN("_BK_ONDERZOEK_NEVENBEVINDING_OPMERKING", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var beoordeling = getMammaBeoordelingMetEersteEnOfTweedeLezing(context);
				if (beoordeling != null && (!beoordeling.getEersteLezing().getNevenbevindingen().isEmpty() || !beoordeling.getTweedeLezing().getNevenbevindingen().isEmpty()))
				{
					return getNevenBevindingenOpmerkingTekst(beoordeling);
				}
				return null;
			}
		},

	MAMMA_BEPERKT_BEOORDEELBAAR_REDEN(
		"_BK_BEPERKT_BEOORDEELBAAR_REDEN",
		MergeFieldTestType.OVERIGE,
		"client.mammaDossier.laatsteScreeningRonde.laatsteOnderzoek.laatsteBeoordeling.eersteLezing.beperktBeoordeelbaarReden",
		MammaBeperktBeoordeelbaarReden.class,
		NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var beoordeling = getMammaBeoordeling(context);
				if (beoordeling != null)
				{
					final var reden = MammaBeoordelingUtil.beperktBeoordeelbaarReden(beoordeling);
					if (reden != null)
					{
						if (reden == MammaBeperktBeoordeelbaarReden.FOTOS_MAAR_IN_1_RICHTING_GEMAAKT)
						{
							return "de röntgenfoto('s) maar in één richting zijn gemaakt";
						}
						else if (reden == MammaBeperktBeoordeelbaarReden.MAMMA_NIET_VOLLEDIG_AFGEBEELD)
						{
							return "uw borst(en) niet volledig is (zijn) afgebeeld";
						}
					}
				}
				return ""; 
			}
		},

	MAMMA_VERSLAG_UITSLAG_CONCLUSIE("_BK_VERSLAG_UITSLAG_CONCLUSIE", NIET_NAAR_INPAKCENTRUM)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var beoordeling = getMammaBeoordeling(context);
				if (beoordeling != null)
				{
					final var reden = MammaBeoordelingUtil.beperktBeoordeelbaarReden(beoordeling);
					if (reden != null)
					{
						switch (reden)
						{
						case KWALITEIT_KOMT_NIET_OVEREEN_MET_STANDAARD:
						case FOTOS_VAN_EEN_OF_BEIDE_ZIJDEN_BEWOGEN:
						case FOTOS_MAAR_IN_1_RICHTING_GEMAAKT:
						case MAMMA_NIET_VOLLEDIG_AFGEBEELD:
							return "Bij dit onderzoek was slechts een gedeeltelijke beoordeling mogelijk. Op het gedeelte dat we wél konden beoordelen, hebben we geen aanwijzingen voor borstkanker gevonden.";

						case PROTHESE_MEER_DAN_0_PUNT_8:
							return "Door de aanwezigheid van prothese(n) was slechts een gedeeltelijke beoordeling mogelijk. Op het gedeelte dat we wél konden beoordelen, hebben we geen aanwijzingen voor borstkanker gevonden.";

						case GEEN_BEOORDELING_MOGELIJK:
							return "Bij dit onderzoek was geen beoordeling mogelijk.";
						}
					}
					else
					{
						return "Bij dit onderzoek zijn geen afwijkingen gevonden.";
					}
				}
				return null;
			}
		},

	MAMMA_UITGESTELD_TEKST("_BK_UITGESTELD_TEKST",
		MergeFieldTestType.OVERIGE, String.class, () -> getStringValueFromPreference(PreferenceKey.MAMMA_UITNODIGING_NA_UITSTEL_TEKST),
		NIET_NAAR_INPAKCENTRUM, NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var brief = getHerdrukOfOrigineleMammaBrief(context);
				if (brief != null)
				{
					var uitnodiging = brief.getUitnodiging();
					if (uitnodigingGevolgVanClientUitstel(uitnodiging))
					{
						return getStringValueFromPreference(PreferenceKey.MAMMA_UITNODIGING_NA_UITSTEL_TEKST);
					}
				}
				return null;
			}

			private boolean uitnodigingGevolgVanClientUitstel(MammaUitnodiging uitnodiging)
			{
				return uitnodiging != null && uitnodiging.getScreeningRonde() != null &&
					uitnodiging.getScreeningRonde().getUitstellen().stream().filter(uitstel -> uitstel.getUitstelReden() == MammaUitstelReden.CLIENT_CONTACT)
						.anyMatch(uitstel -> uitnodiging.equals(uitstel.getUitnodiging()));
			}
		},

	CLIENT_SIGNALERING_GENDER("_CLIENT_SIGNALERING_GENDER", MergeFieldTestType.OVERIGE, String.class, () -> "", NIET_NAAR_INPAKCENTRUM, NIET_IN_HUISARTSBERICHT)
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (context.getClient() == null)
				{
					return null;
				}
				var persoon = context.getClient().getPersoon();
				if (persoon.getGeslacht() == Geslacht.ONBEKEND)
				{
					var auditsVanAnderGeslacht = EntityAuditUtil.getEntityHistory(persoon, getHibernateSession(), AuditEntity.property("geslacht").ne(Geslacht.ONBEKEND), false, 1);
					if (auditsVanAnderGeslacht.isEmpty())
					{
						return getStringValueFromPreference(PreferenceKey.CLIENT_NIEUW_GENDERDIVERS_TEKST);
					}
				}
				return getStringValueFromPreference(PreferenceKey.CLIENT_GENDERIDENTITEITSWIJZIGING_TEKST);
			}
		},

	BEVESTIGING_AFMELDING_TEKST_DK("_BEVESTIGING_AFMELDING_TEKST_DK")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				var dossier = context.getClient().getColonDossier();
				ColonAfmelding afmelding;

				if (DossierStatus.INACTIEF == dossier.getStatus())
				{
					afmelding = AfmeldingUtil.getHeraanmeldbareAfmelding(dossier);
				}
				else
				{
					afmelding = AfmeldingUtil.getLaatsteDefinitieveOfTijdelijkeHeraangemeldeAfmelding(dossier);
				}

				if (AfmeldingType.DEFINITIEF == afmelding.getType())
				{
					return getStringValueFromPreference(PreferenceKey.COLON_DEFINITIEVE_AFMELDING_BEVESTIGING_TEKST);
				}
				else if (AfmeldingType.TIJDELIJK == afmelding.getType())
				{
					var tekstMetJaarInsert = getStringValueFromPreference(PreferenceKey.COLON_TIJDELIJKE_AFMELDING_BEVESTIGING_TEKST);
					String uitstellenTotJaartal;

					var volgendeUitnodiging = dossier.getVolgendeUitnodiging();
					if (volgendeUitnodiging.getDatumVolgendeRonde() == null)
					{
						var vorigeVolgendeUitnodigingVersies = EntityAuditUtil.getEntityHistory(volgendeUitnodiging, MergeField.getHibernateSession(),
							true);
						for (var entiteitGeschiedenis : vorigeVolgendeUitnodigingVersies)
						{
							var vorigeRevisieVolgendeUitnodiging = (ColonVolgendeUitnodiging) entiteitGeschiedenis[0];
							if (vorigeRevisieVolgendeUitnodiging.getDatumVolgendeRonde() != null)
							{
								volgendeUitnodiging = vorigeRevisieVolgendeUitnodiging;
								break;
							}
						}
					}
					uitstellenTotJaartal = String.valueOf(volgendeUitnodiging.getDatumVolgendeRonde().getYear());

					if (tekstMetJaarInsert != null)
					{
						return tekstMetJaarInsert.replace("{jaar}", uitstellenTotJaartal);
					}
				}
				return null;
			}
		},
	GEEN_HANDTEKENING_TEKST("_GEEN_HANDTEKENING_TEKST")
		{
			@Override
			public Object getFieldValue(MailMergeContext context)
			{
				if (!(context.getBrief() instanceof BezwaarBrief) || !((BezwaarBrief) context.getBrief()).isVragenOmHandtekening())
				{
					return null;
				}

				return getStringValueFromPreference(PreferenceKey.GEEN_HANDTEKENING_BRIEF_TEKST);
			}
		};

	@Getter
	private final String fieldName; 

	@Getter
	private Class<?> instance; 

	@Getter
	private MergeFieldTestType type; 

	private String currentValue; 

	private Supplier<String> initialTestValueSupplier; 

	@Setter
	@Getter
	private Class<? extends AbstractBarcodeBean> barcodeType;

	@Getter
	private boolean barcode;

	@Getter
	private boolean qrCode;

	@Getter
	private Double barcodeHeight;

	private final boolean naarInpakcentrum;

	private final boolean inHuisartsenbericht;

	private final boolean waardeTrimmen;

	@Getter
	private String property;

	MergeField(String fieldName, MergeFieldFlag... flags)
	{
		this.fieldName = fieldName;
		var flagList = Arrays.asList(flags);
		naarInpakcentrum = !flagList.contains(NIET_NAAR_INPAKCENTRUM);
		inHuisartsenbericht = !flagList.contains(NIET_IN_HUISARTSBERICHT);
		qrCode = flagList.contains(QR_CODE);
		waardeTrimmen = !flagList.contains(WAARDE_NIET_TRIMMEN);
	}

	MergeField(String fieldName, MergeFieldTestType type, Class<?> instance, Supplier<String> initialTestValueSupplier, MergeFieldFlag... flags)
	{
		this(fieldName, flags);
		this.type = type;
		this.instance = instance;
		this.initialTestValueSupplier = initialTestValueSupplier;
	}

	MergeField(String fieldName, MergeFieldTestType type, String property, Class<?> instance, MergeFieldFlag... flags)
	{
		this(fieldName, flags);
		this.type = type;
		this.instance = instance;
		this.property = property;
	}

	MergeField(String fieldName, Class<? extends AbstractBarcodeBean> barcodeType, Double barcodeHeight, MergeFieldFlag... flags)
	{
		this(fieldName, flags);
		this.barcodeType = barcodeType;
		barcode = true;
		qrCode = false;
		this.barcodeHeight = barcodeHeight;
	}

	MergeField(String fieldName, Class<? extends AbstractBarcodeBean> barcodeType, Double barcodeHeight, MergeFieldTestType type, Class<?> instance,
		Supplier<String> initialTestValueSupplier, MergeFieldFlag... flags)
	{
		this(fieldName, type, instance, initialTestValueSupplier, flags);
		this.barcodeType = barcodeType;
		this.barcodeHeight = barcodeHeight;
		barcode = true;
		qrCode = false;
	}

	MergeField(String fieldName, Class<? extends AbstractBarcodeBean> barcodeType, Double barcodeHeight, MergeFieldTestType type, String property, Class<?> instance,
		MergeFieldFlag... flags)
	{
		this(fieldName, type, property, instance, flags);
		this.barcodeType = barcodeType;
		this.barcodeHeight = barcodeHeight;
		barcode = true;
		qrCode = false;
	}

	public boolean inHuisartsenbericht()
	{
		return inHuisartsenbericht;
	}

	public boolean waardeTrimmen()
	{
		return waardeTrimmen;
	}

	public boolean naarInpakcentrum()
	{
		return naarInpakcentrum;
	}

	public abstract Object getFieldValue(MailMergeContext context);

	public Object getValue(MailMergeContext context)
	{
		if (context.isUseTestValue() && currentValue != null)
		{
			if (Date.class.equals(instance))
			{
				Date datum = null;
				try
				{
					datum = new SimpleDateFormat("dd-MM-yyyy").parse(currentValue);
				}
				catch (ParseException ignored)
				{

				}
				if (datum != null && (equals(CLIENT_GEBOORTEDATUM) || equals(ZV_GEBOORTEDATUM)))
				{
					return new SimpleDateFormat("dd-MM-yyyy").format(datum);
				}
				else if (datum != null && equals(DATUM_VANDAAG))
				{
					return getFormattedDateZonderDagnaam(datum);
				}
				else
				{
					return getFormattedDateMetDagnaam(datum);
				}
			}

			return currentValue;
		}
		return getFieldValue(context);
	}

	public static MergeField getByFieldname(String fieldname)
	{
		for (var mergeField : MergeField.values())
		{
			if (fieldname.equals(mergeField.fieldName))
			{
				return mergeField;
			}
		}
		return null;
	}

	public static List<MergeField> getFieldWithType(MergeFieldTestType type)
	{
		List<MergeField> fields = new ArrayList<>();
		for (var field : MergeField.values())
		{
			if (type.equals(field.getType()))
			{
				fields.add(field);
			}
		}
		return fields;
	}

	public static void resetDefaultMergeFields()
	{
		for (var field : MergeField.values())
		{
			field.currentValue = field.initialTestValueSupplier != null ? field.initialTestValueSupplier.get() : null;
		}
	}

	private static Adres getAdres(Instelling instelling, int index)
	{
		Adres adres = null;
		if (instelling != null)
		{
			adres = getAdresFromList(instelling.getAdressen(), index);
		}
		return adres;
	}

	private static Adres getAdres(Gebruiker gebruiker)
	{
		Adres adres = null;
		if (gebruiker != null)
		{
			adres = getAdresFromList(gebruiker.getAdressen(), 0);
		}
		return adres;
	}

	private static Adres getAdresFromList(List<Adres> adressen, int index)
	{
		Adres adres = null;
		if (adressen != null && adressen.size() >= index + 1)
		{
			adres = adressen.get(index);
		}
		return adres;
	}

	private static Adres getClientAdres(MailMergeContext context)
	{
		var persoon = context.getClient().getPersoon();

		return AdresUtil.getAdres(persoon, LocalDate.now());
	}

	private static Instelling getZorginstellingBijKwaliteitsOvereenkomst(AfgeslotenMedewerkerOvereenkomst afgeslotenOvereenkomst)
	{
		if (CollectionUtils.isNotEmpty(afgeslotenOvereenkomst.getGebruiker().getOrganisatieMedewerkers()))
		{
			for (var orgMede : afgeslotenOvereenkomst.getGebruiker().getOrganisatieMedewerkers())
			{
				if (organisatieMedewerkerActief(orgMede))
				{
					var organisatie = orgMede.getOrganisatie();
					if (organisatie != null && !Boolean.FALSE.equals(organisatie.getActief()))
					{
						var type = organisatie.getOrganisatieType();
						if (OrganisatieType.INTAKELOCATIE == type || OrganisatieType.COLOSCOPIELOCATIE == type)
						{
							var parent = organisatie.getParent();
							if (parent != null && !Boolean.FALSE.equals(parent.getActief()) && OrganisatieType.ZORGINSTELLING == parent.getOrganisatieType())
							{
								return parent;
							}
						}
						else if (OrganisatieType.ZORGINSTELLING == type)
						{
							return organisatie;
						}
					}
				}
			}
		}
		return null;
	}

	private static boolean organisatieMedewerkerActief(InstellingGebruiker orgMede)
	{
		var inDienst = false;
		for (var organisatieMedewerkerRol : orgMede.getRollen())
		{
			if (!Boolean.FALSE.equals(organisatieMedewerkerRol.getActief()))
			{
				var beginDatum = organisatieMedewerkerRol.getBeginDatum();
				var eindDatum = organisatieMedewerkerRol.getEindDatum();
				var vandaag = DateUtil.toUtilDateMidnight(new Date());
				if (beginDatum == null)
				{
					beginDatum = vandaag;
				}
				if (eindDatum == null)
				{
					eindDatum = vandaag;
				}
				if (!beginDatum.after(vandaag) && !eindDatum.before(vandaag))
				{
					inDienst = true;
					break;
				}
			}
		}

		return inDienst && !Boolean.FALSE.equals(orgMede.getActief());
	}

	private static Instelling getZorgInstelling(MailMergeContext context)
	{
		var overeenkomst = context.getOvereenkomst();
		if (overeenkomst instanceof AfgeslotenInstellingOvereenkomst)
		{
			var afgeslotenOvereenkomst = (AfgeslotenInstellingOvereenkomst) context.getOvereenkomst();
			return afgeslotenOvereenkomst.getInstelling();
		}
		else if (overeenkomst instanceof AfgeslotenMedewerkerOvereenkomst)
		{
			var afgeslotenOvereenkomst = (AfgeslotenMedewerkerOvereenkomst) overeenkomst;
			return getZorginstellingBijKwaliteitsOvereenkomst(afgeslotenOvereenkomst);
		}
		return null;
	}

	private static CervixBetaalopdracht getBetaalopdracht(MailMergeContext context)
	{
		return context.getValue(MailMergeContext.CONTEXT_BMHK_BETAALOPDRACHT);
	}

	private static ScreeningOrganisatie getScreeningOrganisatie(MailMergeContext context)
	{
		ScreeningOrganisatie screeningOrganisatie = context.getValue(MailMergeContext.CONTEXT_SCREENING_ORGANISATIE);
		CervixHuisarts cervixHuisarts = context.getValue(MailMergeContext.CONTEXT_CERVIX_HUISARTS);
		CervixHuisartsLocatie cervixHuisartsLocatie = context.getValue(MailMergeContext.CONTEXT_HA_LOCATIE);
		if (cervixHuisartsLocatie != null && cervixHuisartsLocatie.getLocatieAdres() != null && cervixHuisartsLocatie.getLocatieAdres().getWoonplaats() != null
			&& cervixHuisartsLocatie.getLocatieAdres().getWoonplaats().getGemeente() != null)
		{
			screeningOrganisatie = cervixHuisartsLocatie.getLocatieAdres().getWoonplaats().getGemeente().getScreeningOrganisatie();
		}
		else if (cervixHuisarts != null && cervixHuisarts.getPostadres() != null && cervixHuisarts.getPostadres().getWoonplaats() != null
			&& cervixHuisarts.getPostadres().getWoonplaats().getGemeente() != null)
		{
			screeningOrganisatie = cervixHuisarts.getPostadres().getWoonplaats().getGemeente().getScreeningOrganisatie();
		}
		else if (context.getOvereenkomst() != null)
		{
			screeningOrganisatie = context.getOvereenkomst().getScreeningOrganisatie();
		}
		else if (context.getClient() != null)
		{
			screeningOrganisatie = context.getClient().getPersoon().getGbaAdres().getGbaGemeente().getScreeningOrganisatie();
		}
		return screeningOrganisatie;
	}

	private static String getFormattedDateMetDagnaam(LocalDateTime date)
	{
		if (date != null)
		{
			return date.format(DateUtil.LOCAL_DATE_UITGEBREID_DAG_UITEGEBREID_MAAND_FORMAT);
		}
		else
		{
			return null;
		}
	}

	private static String getFormattedDateMetDagnaam(Date date)
	{
		if (date != null)
		{
			return DateUtil.toLocalDate(date).format(DateUtil.LOCAL_DATE_UITGEBREID_DAG_UITEGEBREID_MAAND_FORMAT);
		}
		else
		{
			return null;
		}
	}

	private static String getFormattedDateZonderDagnaam(Date date)
	{
		if (date != null)
		{
			return DateUtil.toLocalDate(date).format(DateUtil.LOCAL_DATE_DAG_UITGEBREID_MAAND_FORMAT);
		}
		else
		{
			return null;
		}
	}

	private static Adres getBmhkRetouradres(CervixUitnodiging uitnodiging)
	{
		return getBmhkRetouradresMetMelding(uitnodiging, new StringBuilder());
	}

	public static Adres getBmhkRetouradresMetMelding(CervixUitnodiging uitnodiging, StringBuilder melding)
	{
		Adres bmhkRetouradres = null;
		if (uitnodiging != null)
		{
			var gbaGemeente = uitnodiging.getScreeningRonde().getDossier().getClient().getPersoon().getGbaAdres().getGbaGemeente();
			var bmhkLaboratorium = gbaGemeente.getBmhkLaboratorium();
			var screeningOrganisatie = gbaGemeente.getScreeningOrganisatie();
			if (bmhkLaboratorium != null)
			{
				for (var retouradres : bmhkLaboratorium.getRetouradressen())
				{
					if (retouradres.getRegio().equals(screeningOrganisatie))
					{
						bmhkRetouradres = retouradres.getAdres();
					}
				}
				if (bmhkRetouradres == null)
				{
					if (screeningOrganisatie != null)
					{
						melding.append("BMHK lab '").append(bmhkLaboratorium.getNaam()).append("' heeft nog geen retouradres voor screeningorganisatie '")
							.append(screeningOrganisatie.getNaam()).append("'. ");
					}
					else
					{
						melding.append("BMHK lab '").append(bmhkLaboratorium.getNaam()).append("' heeft nog geen enkele retouradres voor een screeningorganisatie. ");
					}
				}
			}
			else
			{
				melding.append("Gemeente '").append(gbaGemeente.getNaam()).append("' niet gekoppeld aan BMHK lab. ");
			}
			if (screeningOrganisatie == null)
			{
				melding.append("Gemeente '").append(gbaGemeente.getNaam()).append("' niet gekoppeld aan screeningorganisatie. ");
			}
		}
		return bmhkRetouradres;
	}

	private static Object getBMHKLaboratoriumOndertekenaar(CervixBrief brief, boolean handtekening)
	{
		var monster = brief.getMonster();
		if (monster != null)
		{
			var laboratorium = monster.getLaboratorium();
			var monsterType = monster.getUitnodiging().getMonsterType();
			if (monsterType == CervixMonsterType.UITSTRIJKJE)
			{
				var uitstrijkje = CervixMonsterUtil.getUitstrijkje(monster);
				switch (uitstrijkje.getUitstrijkjeStatus())
				{
				case NIET_ONTVANGEN: 
					laboratorium = uitstrijkje.getLabformulier().getLaboratorium();

				case ONTVANGEN:
				case NIET_ANALYSEERBAAR:
				case GEANALYSEERD_OP_HPV_POGING_1:
				case GEANALYSEERD_OP_HPV_POGING_2:
					return getBMHKLaboratoriumOndertekenaar(laboratorium, handtekening);
				case BEOORDEELD_DOOR_CYTOLOGIE:
					return handtekening ? laboratorium.getHandtekeningPatholoog() : laboratorium.getPatholoog();
				default:
					throw new IllegalStateException();
				}
			}
			else if (monsterType == CervixMonsterType.ZAS)
			{
				var zas = CervixMonsterUtil.getZAS(monster);
				switch (zas.getZasStatus())
				{
				case ONTVANGEN:
				case NIET_ANALYSEERBAAR:
				case GEANALYSEERD_OP_HPV_POGING_1:
				case GEANALYSEERD_OP_HPV_POGING_2:
					return getBMHKLaboratoriumOndertekenaar(laboratorium, handtekening);
				default:
					throw new IllegalStateException();
				}
			}
		}

		var labformulier = brief.getLabformulier();
		if (labformulier != null)
		{
			var laboratorium = labformulier.getLaboratorium();
			return getBMHKLaboratoriumOndertekenaar(laboratorium, handtekening);
		}

		var herdruk = BriefUtil.getHerdruk(brief);
		if (herdruk != null)
		{
			return getBMHKLaboratoriumOndertekenaar((CervixBrief) herdruk, handtekening);
		}
		return null;
	}

	private static Object getBMHKLaboratoriumOndertekenaar(BMHKLaboratorium laboratorium, boolean handtekening)
	{
		return handtekening ? laboratorium.getHandtekeningMedischMircobioloog() : laboratorium.getMedischMircobioloog();
	}

	private static String getValueVanHeraanmeldenTekstKey(ClientBrief<?, ?, ?> brief)
	{
		var heraanmeldenMergeVeldService = getBean(HeraanmeldenMergeVeldService.class);
		return heraanmeldenMergeVeldService.getValueVanHeraanmeldenTekstKey(brief);
	}

	private static CervixUitnodiging getOorspronkelijkeCervixUitnoding(CervixUitnodiging uitnodiging)
	{
		if (uitnodiging != null)
		{
			var brief = uitnodiging.getBrief();
			if (brief != null && !Boolean.TRUE.equals(uitnodiging.getHerinnering()) && !Boolean.TRUE.equals(uitnodiging.getUitgesteld()))
			{
				var herdruk = (CervixBrief) BriefUtil.getHerdruk(brief);
				if (herdruk != null)
				{
					return getOorspronkelijkeCervixUitnoding(herdruk.getUitnodiging());
				}
			}
		}
		return uitnodiging;
	}

	private static MammaStandplaatsLocatie getMammaStandplaatsLocatieAfspraak(MailMergeContext context)
	{
		var brief = BriefUtil.getOrigineleBrief(context.getBrief());
		var afspraakService = getBean(MammaBaseAfspraakService.class);
		var afspraak = afspraakService.getLaatsteAfspraakVanBriefronde(brief);

		var locatie = afspraakService.getMammaStandplaatsLocatieAfspraak(afspraak);
		if (locatie == null)
		{
			locatie = getMammaStandplaatsLocatieUitnodiging(context.getClient().getMammaDossier().getLaatsteScreeningRonde());
		}
		return locatie;
	}

	private static MammaStandplaatsLocatie getMammaStandplaatsLocatieUitnodiging(MammaScreeningRonde ronde)
	{
		var afspraakService = getBean(MammaBaseAfspraakService.class);
		var uitnodiging = afspraakService.getLaatsteUitnodigingVanScreeningRonde(ronde);

		return afspraakService.getMammaStandplaatsLocatieUitnodiging(uitnodiging);
	}

	private static String getMammaUitnodigingsNummer(MailMergeContext context)
	{
		var mammaBrief = getOrigineleMammaBrief(context);
		if (mammaBrief != null && mammaBrief.getScreeningRonde() != null)
		{
			return mammaBrief.getScreeningRonde().getUitnodigingsNr().toString();
		}
		var laatsteAfspraak = getLaatsteMammaAfspraak(context);
		if (laatsteAfspraak != null)
		{
			return laatsteAfspraak.getUitnodiging().getScreeningRonde().getUitnodigingsNr().toString();
		}
		return null;
	}

	private static CentraleEenheid getMammaCentraleEenheid(MailMergeContext context)
	{
		return context.getValue(MailMergeContext.CONTEXT_MAMMA_CE);
	}

	private static Gebruiker getMammaRadioloog1(MailMergeContext context)
	{
		var mergeFieldService = getBean(MammaMergeFieldService.class);
		var beoordeling = getMammaBeoordeling(context);

		return mergeFieldService.bepaalRadioloog1(beoordeling);
	}

	private static Gebruiker getMammaRadioloog2(MailMergeContext context)
	{
		var mergeFieldService = getBean(MammaMergeFieldService.class);
		var beoordeling = getMammaBeoordeling(context);

		return mergeFieldService.bepaalRadioloog2(beoordeling);
	}

	public static String bepaalVerslagBiradsTekst(MammaLezing verslagLezing, MammaZijde zijde)
	{
		var baseBeoordelingService = getBean(MammaBaseBeoordelingService.class);
		if (verslagLezing == null)
		{
			return null;
		}
		if (baseBeoordelingService.iBiradsWaardeGeen(verslagLezing, zijde))
		{
			var amputatie = verslagLezing.getBeoordeling().getOnderzoek().getAmputatie();
			if (MammaZijde.RECHTER_BORST.equals(zijde) && MammaAmputatie.RECHTERBORST.equals(amputatie))
			{
				return "BI-RADS rechts: geen beoordeling vanwege amputatie";
			}
			else if (MammaZijde.LINKER_BORST.equals(zijde) && MammaAmputatie.LINKERBORST.equals(amputatie))
			{
				return "BI-RADS links: geen beoordeling vanwege amputatie";
			}
			else
			{
				return null;
			}
		}
		return MammaZijde.RECHTER_BORST.equals(zijde) ? "BI-RADS rechts: " + verslagLezing.getBiradsRechts().getNaam()
			: "BI-RADS links: " + verslagLezing.getBiradsLinks().getNaam();
	}

	private static MammaBeoordeling getMammaBeoordeling(MailMergeContext context)
	{
		MammaBeoordeling beoordeling = context.getValue(MailMergeContext.CONTEXT_MAMMA_BEOORDELING);
		if (beoordeling != null)
		{
			return beoordeling;
		}
		else if (context.getClient() != null)
		{
			return MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(context.getClient());
		}
		return null;
	}

	protected MammaBeoordeling getMammaBeoordelingMetEersteEnOfTweedeLezing(MailMergeContext context)
	{
		var laatsteAfspraak = getAfspraakVanLaatsteOnderzoek(context);
		var beoordelingService = getBean(MammaBaseBeoordelingService.class);
		return beoordelingService.getBeoordelingMetEersteEnOfTweedeLezing(laatsteAfspraak);
	}

	private static MammaAfspraak getAfspraakVanLaatsteOnderzoek(MailMergeContext context)
	{
		if (context.getClient() != null)
		{
			return MammaScreeningRondeUtil.getAfspraakVanLaatsteOnderzoek(context.getClient().getMammaDossier());
		}
		return null;
	}

	private static MammaAfspraak getLaatsteMammaAfspraak(MailMergeContext context)
	{
		var brief = BriefUtil.getOrigineleBrief(context.getBrief());
		if (brief instanceof MammaBrief)
		{
			var afspraakService = getBean(MammaBaseAfspraakService.class);
			return afspraakService.getLaatsteAfspraakVanBriefronde(brief);
		}
		else if (context.getClient() != null)
		{
			var dossier = context.getClient().getMammaDossier();
			if (dossier.getLaatsteScreeningRonde() != null && dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging() != null)
			{
				return dossier.getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak();
			}
		}
		return null;
	}

	private static MammaLezing getVerslagLezing(MailMergeContext context)
	{
		var beoordelingService = getBean(MammaBaseBeoordelingService.class);
		var beoordeling = beoordelingService.getBeoordelingMetVerslagLezing(getAfspraakVanLaatsteOnderzoek(context));
		if (beoordeling != null && beoordeling.getVerslagLezing() != null)
		{
			beoordeling.getVerslagLezing().setBeoordeling(beoordeling);
			return beoordeling.getVerslagLezing();
		}
		return null;
	}

	private static String createLaesiesTekst(MammaLezing verslaglezing)
	{
		if (verslaglezing == null)
		{
			return null;
		}
		final var laesieService = getBean(MammaBaseLaesieService.class);
		return laesieService.getAllLaesieTekstVoorVerslagLezing(verslaglezing);
	}

	private static String getNevenBevindingenOpmerkingTekst(MammaBeoordeling beoordeling)
	{
		if (beoordeling == null)
		{
			return null;
		}
		var beoordelingService = getBean(MammaBaseBeoordelingService.class);
		var nevenbevindingOpmerkingTekst = beoordelingService.getNevenbevindingOpmerkingTekst("\n", beoordeling.getEersteLezing(), beoordeling.getTweedeLezing());
		return nevenbevindingOpmerkingTekst != null ? "Nevenbevindingen opmerking(en):" + nevenbevindingOpmerkingTekst : null;
	}

	public static String getNevenbevindingenEnumString(MammaBeoordeling beoordeling)
	{
		var beoordelingService = getBean(MammaBaseBeoordelingService.class);
		return String.format("\nNevenbevindingen:\n%s.\n",
			beoordelingService.getMammaLezingEnumsTekst(MammaLezing::getNevenbevindingen, beoordeling.getEersteLezing(), beoordeling.getTweedeLezing()));
	}

	public String getValueTekstKeyAlsHeraangemeldeUitnodigingVoorInpakcentrum(List<ColonUitnodiging> colonUitnodigingen)
	{
		if (colonUitnodigingen != null && colonUitnodigingen.size() > 1)
		{
			colonUitnodigingen = new ArrayList<>(colonUitnodigingen);
			colonUitnodigingen.sort(new PropertyComparator<>("creatieDatum", false, false));
			var eenNaLaatsteUitnodiging = colonUitnodigingen.get(1);
			if (eenNaLaatsteUitnodiging.getGekoppeldeTest() != null)
			{
				var heraanmeldenTekstKey = eenNaLaatsteUitnodiging.getGekoppeldeTest().getHeraanmeldenTekstKey();
				if (heraanmeldenTekstKey != null)
				{
					return getStringValueFromPreference(heraanmeldenTekstKey);
				}
			}
		}
		return null;

	}

	private static MammaAfspraak getEenNaLaatsteAfspraak(MammaUitnodiging laatsteUitnodiging)
	{
		return laatsteUitnodiging.getAfspraken().stream().filter(a -> a.getId() != null && !a.getId().equals(laatsteUitnodiging.getLaatsteAfspraak().getId()))
			.max(Comparator.comparing(MammaAfspraak::getCreatiedatum)).orElse(null);
	}

	private static MammaBrief getOrigineleMammaBrief(MailMergeContext context)
	{
		var brief = BriefUtil.getOrigineleBrief(context.getBrief());
		if (brief instanceof MammaBrief)
		{
			return (MammaBrief) brief;
		}
		return null;
	}

	private static MammaBrief getHerdrukOfOrigineleMammaBrief(MailMergeContext context)
	{
		var brief = getOrigineleMammaBrief(context);
		if (brief != null)
		{
			return BriefUtil.isHerdruk(brief) ? (MammaBrief) BriefUtil.getHerdruk(brief) : brief;
		}
		return null;
	}

	private static CervixLeeftijdcategorie getCervixLeeftijdcategorie(MailMergeContext context)
	{
		var brief = BriefUtil.getOrigineleBrief(context.getBrief());
		if (brief instanceof CervixBrief)
		{
			var cervixBrief = (CervixBrief) brief;
			return cervixBrief.getScreeningRonde().getLeeftijdcategorie();
		}
		return null;
	}

	private static <T> T getBean(Class<T> clazz)
	{
		return ApplicationContextProvider.getApplicationContext().getBean(clazz);
	}

	private static SimplePreferenceService getSimplePreferenceService()
	{
		return getBean(SimplePreferenceService.class);
	}

	private static OrganisatieParameterService getOrganisatieParameterService()
	{
		return getBean(OrganisatieParameterService.class);
	}

	private static String getStringValueFromPreference(PreferenceKey key)
	{
		var preferenceService = getSimplePreferenceService();
		return preferenceService.getString(key.name(), "");
	}

	private static Session getHibernateSession()
	{
		return getBean(HibernateService.class).getHibernateSession();
	}

}
