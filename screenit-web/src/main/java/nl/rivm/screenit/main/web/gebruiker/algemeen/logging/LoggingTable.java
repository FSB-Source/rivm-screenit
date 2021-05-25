package nl.rivm.screenit.main.web.gebruiker.algemeen.logging;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.mamma.MammaHl7v24BerichtPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.brievengenereren.BrievenGenererenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cdaverslag.CdaVerslagErrorDownloadCdaPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.gevolgenlabprocesverwerken.GevolgenLabprocesVerwerkenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.herinneren.HerinnerenVerwerkenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.huisartsberichten.HuisartsberichtenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.cervix.zas.ZasVersturenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.gba.GbaVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.ifobtverwerking.IfobtVerwerkingVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.intake.IntakeVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.ilm.MammaIlmBeeldenStatusRapportagePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.uitnodigingen.MammaUitnodigenRapportagePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.retourzendingen.RetourzendingenVerwerkingsVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.selectie.SelectieVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.uitnodigingversturen.UitnodigingVersturenVerslagPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.palga.MammaPalgaUitwisselingPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.logging.BerichtOntvangenLogEvent;
import nl.rivm.screenit.model.logging.BrievenGenererenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.CervixHerinnerenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.CervixHuisartsberichtenBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.CervixUitnodigingVersturenLogEvent;
import nl.rivm.screenit.model.logging.GbaVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.IfobtVerwerkingBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.IntakeMakenLogEvent;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.model.logging.LogRegel;
import nl.rivm.screenit.model.logging.MammaHl7v24BerichtLogEvent;
import nl.rivm.screenit.model.logging.MammaIlmLogEvent;
import nl.rivm.screenit.model.logging.MammaUitnodigenLogEvent;
import nl.rivm.screenit.model.logging.RetourzendingLogEvent;
import nl.rivm.screenit.model.logging.SelectieRondeBeeindigdLogEvent;
import nl.rivm.screenit.model.logging.UitnodigingVersturenLogEvent;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.ComponentTag;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.hibernate.proxy.HibernateProxy;

public class LoggingTable extends ScreenitDataTable<LogRegel, String>
{
	public LoggingTable(String id, List<IColumn<LogRegel, String>> columns, SortableDataProvider<LogRegel, String> dataProvider, int nrOfRows)
	{
		super(id, columns, dataProvider, nrOfRows, Model.of("logregels"));
	}

	@Override
	public void onClick(AjaxRequestTarget target, IModel<LogRegel> model)
	{
		LogRegel logRegel = model.getObject();
		LogEvent logEvent = logRegel.getLogEvent();
		if (logEvent instanceof HibernateProxy)
		{
			HibernateProxy hibernateProxy = (HibernateProxy) logEvent;
			logEvent = (LogEvent) hibernateProxy.getHibernateLazyInitializer().getImplementation();
		}
		switch (logRegel.getLogGebeurtenis())
		{
		case GBA_IMPORT_AFGEROND:
			setResponsePage(new GbaVerslagPage(ModelUtil.sModel(((GbaVerwerkingBeeindigdLogEvent) logEvent).getVerwerkingsLog())));
			break;
		case INTAKE_AFSPRAAK_MAKEN_AFGEROND:
			setResponsePage(new IntakeVerslagPage(ModelUtil.sModel((IntakeMakenLogEvent) logEvent), logRegel.getGebeurtenisDatum()));
			break;
		case SELECTIERONDE_BEEINDIGD:
			setResponsePage(new SelectieVerslagPage(ModelUtil.sModel(((SelectieRondeBeeindigdLogEvent) logEvent).getRapportage())));
			break;
		case UITNODIGING_VERSTUREN_JOB_AFGEROND:
			setResponsePage(new UitnodigingVersturenVerslagPage(ModelUtil.sModel(((UitnodigingVersturenLogEvent) logEvent).getRapportage())));
			break;
		case RETOURZENDINGEN_VERWERKT:
			setResponsePage(new RetourzendingenVerwerkingsVerslagPage(ModelUtil.csModel((RetourzendingLogEvent) logEvent)));
			break;
		case IFOBT_VERWERKING_AFGEROND:
		case IFOBT_INLEZEN_AFGEROND:
			setResponsePage(new IfobtVerwerkingVerslagPage(ModelUtil.sModel((IfobtVerwerkingBeeindigdLogEvent) logEvent), logRegel.getLogGebeurtenis()));
			break;
		case BRIEVEN_GENERENEN_AFGEROND:
		case CERVIX_BRIEVEN_GENERENEN_AFGEROND:
		case MAMMA_BRIEVEN_GENEREREN_AFGEROND:
		case PROJECT_BRIEVEN_BATCH_AFGEROND:
		case ALGEMENE_BRIEVEN_BATCH_AFGEROND:
		case BEZWAAR_BRIEVEN_BATCH_AFGEROND:
			setResponsePage(
				new BrievenGenererenVerslagPage(ModelUtil.sModel(((BrievenGenererenBeeindigdLogEvent) logEvent).getRapportage())));
			break;
		case BERICHT_VERWERKT_MET_ERROR:
		case BERICHT_VERWERKT_MET_MELDING:
			setResponsePage(new CdaVerslagErrorDownloadCdaPage(ModelUtil.csModel((BerichtOntvangenLogEvent) logEvent)));
			break;
		case CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_AFGEROND:
			setResponsePage(new GevolgenLabprocesVerwerkenVerslagPage(ModelUtil.sModel(((CervixGevolgenLabprocesVerwerkenBeeindigdLogEvent) logEvent).getRapportage())));
			break;
		case CERVIX_HERINNEREN_BEEINDIGD:
			setResponsePage(new HerinnerenVerwerkenVerslagPage(ModelUtil.sModel(((CervixHerinnerenBeeindigdLogEvent) logEvent).getRapportage())));
			break;
		case CERVIX_HUISARTSBERICHTEN_AFGEROND:
			setResponsePage(new HuisartsberichtenVerslagPage(ModelUtil.sModel(((CervixHuisartsberichtenBeeindigdLogEvent) logEvent).getRapportage())));
			break;
		case CERVIX_ZAS_UITNODIGING_VERSTUREN_JOB_AFGEROND:
			setResponsePage(new ZasVersturenVerslagPage(ModelUtil.sModel((CervixUitnodigingVersturenLogEvent) logEvent)));
			break;
		case MAMMA_HL7_BERICHT_AL_ONTVANGEN:
		case MAMMA_HL7_BERICHT_ONTVANGEN_MISLUKT:
		case MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT:
		case MAMMA_HL7_BERICHT_BEELDEN_VERWIJDERD:
		case MAMMA_HL7_BERICHT_ERROR_ONTVANGEN:
			setResponsePage(new MammaHl7v24BerichtPage(ModelUtil.sModel((MammaHl7v24BerichtLogEvent) logEvent)));
			break;
		case MAMMA_UITNODIGEN_AFGEROND:
			setResponsePage(new MammaUitnodigenRapportagePage(ModelUtil.sModel(((MammaUitnodigenLogEvent) logEvent).getRapportage())));
			break;
		case MAMMA_PALGA_CSV_EXPORT_AFGEROND:
			setResponsePage(new MammaPalgaUitwisselingPage());
			break;
		case MAMMA_ILM_AFGEROND:
			setResponsePage(new MammaIlmBeeldenStatusRapportagePage(new SimpleHibernateModel<>((MammaIlmLogEvent) logEvent)));
			break;
		default:
			Client client = logRegel.getClient();
			if (client != null && !GbaStatus.BEZWAAR.equals(client.getGbaStatus()) && !GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
			{
				setResponsePage(new ClientInzienPage(ModelUtil.sModel(client)));
			}
			else
			{
				((GebruikerBasePage) getPage()).getDialog().openWith(target, new ShowVolledigeLoggingMeldingPopupPanel(BootstrapDialog.CONTENT_ID, logEvent.getVolledigeMelding()));
			}
			break;
		}
	}

	@Override
	protected boolean isRowClickable(IModel<LogRegel> model)
	{

		LogRegel logRegel = model.getObject();
		if (logRegel.getLogEvent() != null)
		{
			switch (logRegel.getLogGebeurtenis())
			{
			case GBA_IMPORT_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(GbaVerslagPage.class);
			case INTAKE_AFSPRAAK_MAKEN_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(IntakeVerslagPage.class);
			case SELECTIERONDE_BEEINDIGD:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(SelectieVerslagPage.class);
			case UITNODIGING_VERSTUREN_JOB_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(UitnodigingVersturenVerslagPage.class);
			case CERVIX_ZAS_UITNODIGING_VERSTUREN_JOB_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ZasVersturenVerslagPage.class);
			case RETOURZENDINGEN_VERWERKT:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(RetourzendingenVerwerkingsVerslagPage.class);
			case IFOBT_VERWERKING_AFGEROND:
			case IFOBT_INLEZEN_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(IfobtVerwerkingVerslagPage.class);
			case BRIEVEN_GENERENEN_AFGEROND:
			case CERVIX_BRIEVEN_GENERENEN_AFGEROND:
			case MAMMA_BRIEVEN_GENEREREN_AFGEROND:
			case PROJECT_BRIEVEN_BATCH_AFGEROND:
			case ALGEMENE_BRIEVEN_BATCH_AFGEROND:
			case BEZWAAR_BRIEVEN_BATCH_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(BrievenGenererenVerslagPage.class);
			case BERICHT_VERWERKT_MET_ERROR:
			case BERICHT_VERWERKT_MET_MELDING:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(CdaVerslagErrorDownloadCdaPage.class);
			case CERVIX_GEVOLGEN_LABPROCES_VERWERKEN_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(GevolgenLabprocesVerwerkenVerslagPage.class);
			case CERVIX_HERINNEREN_BEEINDIGD:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(HerinnerenVerwerkenVerslagPage.class);
			case CERVIX_HUISARTSBERICHTEN_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(HuisartsberichtenVerslagPage.class);
			case MAMMA_HL7_BERICHT_AL_ONTVANGEN:
			case MAMMA_HL7_BERICHT_ONTVANGEN_MISLUKT:
			case MAMMA_HL7_BERICHT_VERSTUREN_MISLUKT:
			case MAMMA_HL7_BERICHT_BEELDEN_VERWIJDERD:
			case MAMMA_HL7_BERICHT_ERROR_ONTVANGEN:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(MammaHl7v24BerichtPage.class);
			case MAMMA_UITNODIGEN_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(MammaUitnodigenRapportagePage.class);
			case MAMMA_PALGA_CSV_EXPORT_AFGEROND:
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(MammaPalgaUitwisselingPage.class);
			case MAMMA_ILM_AFGEROND:
				LogEvent logEvent = logRegel.getLogEvent();
				if (logEvent instanceof HibernateProxy)
				{
					HibernateProxy hibernateProxy = (HibernateProxy) logEvent;
					logEvent = (LogEvent) hibernateProxy.getHibernateLazyInitializer().getImplementation();
				}
				MammaIlmLogEvent mammaIlmLogEvent = (MammaIlmLogEvent) logEvent;
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(MammaIlmBeeldenStatusRapportagePage.class)
					&& mammaIlmLogEvent.getRapportage().getAantalBeelden() > 0;
			default:
				Client client = logRegel.getClient();
				if (client != null && !GbaStatus.BEZWAAR.equals(client.getGbaStatus()) && !GbaStatus.AFGEVOERD.equals(client.getGbaStatus()))
				{
					return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientInzienPage.class);
				}
				return logRegel.getLogEvent() != null && StringUtils.isNotBlank(logRegel.getLogEvent().getVolledigeMelding());
			}
		}
		return false;
	}

	@Override
	protected Item<LogRegel> newRowItem(String id, int index, IModel<LogRegel> model)
	{
		super.newRowItem(id, index, model);
		return new Item<LogRegel>(id, index, model)
		{
			@Override
			protected void onComponentTag(ComponentTag tag)
			{
				super.onComponentTag(tag);
				LogEvent logEvent = getModel().getObject().getLogEvent();
				Level level = Level.INFO;
				if (logEvent != null && logEvent.getLevel() != null)
				{
					level = logEvent.getLevel();
				}
				tag.put("class", "log-" + level.toString().toLowerCase());
			}
		};
	}
}
