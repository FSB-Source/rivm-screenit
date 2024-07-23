
package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.main.service.colon.ColonDossierService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.dossier.ClientDossierPage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.colon.ColonIntakeAfspraak;
import nl.rivm.screenit.model.colon.ColonOnderzoeksVariant;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonUitnodiging;
import nl.rivm.screenit.model.colon.IFOBTTest;
import nl.rivm.screenit.model.colon.SAFTransactionTrail;
import nl.rivm.screenit.model.colon.ScannedAntwoordFormulier;
import nl.rivm.screenit.model.colon.enums.ColonAfmeldingReden;
import nl.rivm.screenit.model.colon.enums.IFOBTTestStatus;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.colon.ColonBaseFITService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.FITTestUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.BooleanLabel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_CLIENT_SR_AANVRAAGFORMULIER_ONTVANGEN,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class AntwoordformulierOntvangenPanel extends AbstractGebeurtenisDetailPanel
{
	@SpringBean
	private ColonDossierService colonDossierService;

	@SpringBean
	private ColonBaseFITService colonFitService;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	private final BootstrapDialog confirmDialog;

	private InfoFragment infoFragment;

	public AntwoordformulierOntvangenPanel(String id, IModel<ScreeningRondeGebeurtenis> model)
	{
		super(id, model);
		ColonUitnodiging uitnodiging = (ColonUitnodiging) model.getObject().getUitnodiging();
		PropertyModel<ColonUitnodiging> uitnodigingModel = new PropertyModel<ColonUitnodiging>(model, "uitnodiging");

		UploadDocument formulier = uitnodiging.getAntwoordFormulier().getFormulier();
		File file = formulier != null ? uploadDocumentService.load(formulier) : null;

		if (file != null && file.exists())
		{

			add(new OrionResourceLink("download", Model.of(file)));
			add(new OrionViewerContainer("aanvraagformulier", Model.of(file)));

		}
		else
		{

			String objid = uitnodiging.getAntwoordFormulier().getObjid();
			throw new IllegalStateException("Opgevraagde formulier met objid " + objid + " bestaat niet in de filestore.");
		}

		infoFragment = new InfoFragment("info",
			new CompoundPropertyModel<>(new PropertyModel<>(model, "uitnodiging.antwoordFormulier")), uitnodigingModel);
		infoFragment.setOutputMarkupId(true);
		add(infoFragment);
		confirmDialog = new BootstrapDialog("confirmDialog");
		add(confirmDialog);
	}

	@Override
	protected void addButton(String id, GebeurtenisPopupBasePanel parent)
	{
		ConfirmingIndicatingAjaxLink<Void> button = new ConfirmingIndicatingAjaxLink<Void>(id, confirmDialog, "label.antwoordformulier.verwijderen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				ColonUitnodiging uitnodiging = (ColonUitnodiging) AntwoordformulierOntvangenPanel.this.getModelObject().getUitnodiging();
				colonDossierService.verwijderScannedAntwoordFormulier(uitnodiging, ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().info(AntwoordformulierOntvangenPanel.this.getString("antwoordformulier.verwijderd"));
				setResponsePage(new ClientDossierPage(ModelUtil.sModel(uitnodiging.getScreeningRonde().getDossier().getClient())));
			}
		};
		button.add(new Label("label", getString("label.verwijderen")));
		button.add(new AttributeAppender("class", Model.of(" btn-danger")));
		ScreeningRondeGebeurtenis screeningRondeGebeurtenis = getModelObject();
		boolean magVerwijderen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_AANVRAAGFORMULIER_ONTVANGEN, Actie.VERWIJDEREN);
		ColonUitnodiging uitnodiging = (ColonUitnodiging) screeningRondeGebeurtenis.getUitnodiging();
		if (uitnodiging != null)
		{
			ColonScreeningRonde colonScreeningRonde = uitnodiging.getScreeningRonde();
			if (uitnodiging.getOnderzoeksVariant() != ColonOnderzoeksVariant.STANDAARD)
			{

				magVerwijderen &= false;
			}
			if (!uitnodiging.equals(colonScreeningRonde.getLaatsteUitnodiging()))
			{

				magVerwijderen &= false;
			}
			ColonIntakeAfspraak laatsteAfspraak = colonScreeningRonde.getLaatsteAfspraak();
			if (laatsteAfspraak != null && laatsteAfspraak.getConclusie() != null)
			{

				magVerwijderen &= false;
			}
			IFOBTTest buis = FITTestUtil.getFITTest(uitnodiging);
			if (buis != null)
			{
				IFOBTTestStatus testStatus = buis.getStatus();
				if (IFOBTTestStatus.ACTIEF.equals(testStatus) || IFOBTTestStatus.VERWIJDERD.equals(testStatus))
				{

					magVerwijderen &= false;
				}
			}
			if (ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER.equals(uitnodiging.getAntwoordFormulier().getStatus()))
			{
				magVerwijderen &= false;
			}
		}
		button.setVisible(magVerwijderen);
		parent.add(button);
	}

	private class InfoFragment extends Fragment
	{
		private final IModel<ColonUitnodiging> uitnodiging;

		public InfoFragment(String id, IModel<ScannedAntwoordFormulier> model, PropertyModel<ColonUitnodiging> uitnodigingModel)
		{
			super(id, "gescandeWaarden", AntwoordformulierOntvangenPanel.this, model);
			this.uitnodiging = uitnodigingModel;
		}

		@Override
		protected void onInitialize()
		{
			super.onInitialize();
			add(DateLabel.forDatePattern("afnameDatum", "dd-MM-yyyy"));
			add(new Label("telefoonNummer1"));
			add(new Label("telefoonNummer2"));
			add(new BooleanLabel("eenmaligAfmelden"));
			add(new BooleanLabel("definitiefAfmelden"));
			add(new BooleanLabel("handtekening"));
			add(new BooleanLabel("meedoenBVOenWO").setVisible(uitnodiging.getObject().getOnderzoeksVariant() != ColonOnderzoeksVariant.STANDAARD));
			add(new BooleanLabel("toestemmingInzage"));
			add(new BooleanLabel("toestemmingBewaren"));
			ScannedAntwoordFormulier saf = (ScannedAntwoordFormulier) getDefaultModelObject();
			Label statusLabel = new Label("statusLabel");

			if (ScannedAntwoordFormulier.STATUS_VERWIJDERD_UIT_DOSSIER.equals(saf.getStatus()))
			{
				statusLabel.setDefaultModel(Model.of(""));
			}
			else
			{
				statusLabel.setDefaultModel(Model.of("Resultaat OCR"));
			}
			add(statusLabel);
			add(new Label("status", getString("OCR." + saf.getStatus(), null, "onbekende status")));
			add(new Label("ifobtStatus", getIfobtStatus(uitnodiging.getObject())).setVisible(ScreenitSession.get().checkPermission(Recht.TESTEN, Actie.INZIEN)));
			int indexAfmeldReden = saf.getIndexAfmeldReden();
			add(new Label("afmeldReden", getString(EnumStringUtil.getPropertyString(ColonAfmeldingReden.resolveEnum(indexAfmeldReden)))));

			List<SAFTransactionTrail> trails = saf.getTransactionTrails();
			Comparator<SAFTransactionTrail> com = (o1, o2) -> o2.getDatumTijd().compareTo(o1.getDatumTijd());

			trails.sort(com);
			ListView<SAFTransactionTrail> listView = new ListView<>("transactionTrails", ModelUtil.listModel(trails))
			{
				@Override
				protected void populateItem(ListItem<SAFTransactionTrail> item)
				{
					IModel<SAFTransactionTrail> model = item.getModel();
					item.add(DateLabel.forDatePattern("datum", new PropertyModel<Date>(model, "datumTijd"), "dd-MM-yyyy HH:mm"));
					item.add(new Label("gebruiker", new PropertyModel<String>(model, "gebruiker")));
					item.add(new Label("bericht", getString(model.getObject().getTransactionId(), model, model.getObject().getTransactionId())));
				}
			};
			add(listView);
		}

		private String getIfobtStatus(ColonUitnodiging uitnodiging)
		{
			IFOBTTest test = FITTestUtil.getFITTest(uitnodiging);
			return getString(EnumStringUtil.getPropertyString(test.getStatus()));
		}
	}
}
