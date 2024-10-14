package nl.rivm.screenit.main.web.gebruiker.clienten.contact.gen;

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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.bezwaar.edit.BezwaarEditPanel;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.ClientContactManier;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarGroupViewWrapper;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.FileStoreLocation;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.DropDownChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public class ClientContactBezwaarPanel extends AbstractClientContactActiePanel<ClientContactActie>
{
	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private HibernateService hibernateService;

	private final IModel<BezwaarMoment> bezwaarMomentModel;

	private final IModel<Client> clientModel;

	private final IModel<List<FileUpload>> files = new ListModel<>();

	private List<BezwaarGroupViewWrapper> wrappers;

	private IModel<UploadDocument> document;

	private final IModel<Boolean> bezwaarAanvragenMetHandtekeningHerinnering = Model.of(false);

	public ClientContactBezwaarPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;
		bezwaarMomentModel = ModelUtil.cModel(new BezwaarMoment());
		BezwaarMoment modelObject = bezwaarMomentModel.getObject();
		modelObject.setManier(ClientContactManier.FORMULIER_VOOR_AANVRAGEN);
		modelObject.setClient(clientModel.getObject());

		List<ClientContactManier> clientContactManieren = new ArrayList<ClientContactManier>(Arrays.asList(ClientContactManier.values()));
		DropDownChoice<ClientContactManier> manier = new ScreenitDropdown<ClientContactManier>("manier", new PropertyModel<>(bezwaarMomentModel, "manier"),
			new ListModel<>(clientContactManieren), new EnumChoiceRenderer<>(this));
		manier.setOutputMarkupPlaceholderTag(true);
		manier.setRequired(true);
		manier.setLabel(Model.of("Manier van afmelden"));
		add(manier);

		var bezwaarMakenContainer = new WebMarkupContainer("bezwaarMakenContainer");
		var directBezwaarMakenContainer = getDirectBezwaarMakenContainer();
		bezwaarMakenContainer.add(directBezwaarMakenContainer);
		var aanvraagFormulierBezwaarMakenContainer = getAanvraagFormulierBezwaarMakenContainer();
		bezwaarMakenContainer.add(aanvraagFormulierBezwaarMakenContainer);
		add(bezwaarMakenContainer);

		manier.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				boolean direct = ClientContactManier.DIRECT.equals(bezwaarMomentModel.getObject().getManier());
				directBezwaarMakenContainer.setVisible(direct);
				aanvraagFormulierBezwaarMakenContainer.setVisible(!direct);
				target.add(directBezwaarMakenContainer);
				target.add(aanvraagFormulierBezwaarMakenContainer);
			}
		});
	}

	private WebMarkupContainer getDirectBezwaarMakenContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("directBezwaarMakenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(false);

		FileUploadField uploadField = new FileUploadField("bestandSelecteren", files);
		uploadField.add(new FileValidator(FileType.PDF));
		uploadField.setRequired(true);
		uploadField.setLabel(Model.of("Bestand"));
		container.add(uploadField);

		BezwaarMoment laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();
		wrappers = bezwaarService.getEditBezwaarGroupViewWrappers(clientModel.getObject(), laatsteVoltooideBezwaarMoment, true);
		container.add(new BezwaarEditPanel("bezwaarAanpassenPanel", wrappers, true));

		return container;
	}

	private WebMarkupContainer getAanvraagFormulierBezwaarMakenContainer()
	{
		var container = new WebMarkupContainer("bezwaarAanvragenContainer");
		container.setOutputMarkupPlaceholderTag(true);
		container.setVisible(true);

		var bezwaarAanvragenRadioChoice = new RadioChoice<>("bezwaarAanvragen", bezwaarAanvragenMetHandtekeningHerinnering,
			new ListModel<>(Arrays.asList(Boolean.FALSE, Boolean.TRUE)),
			new ChoiceRenderer<>()
			{
				@Override
				public Object getDisplayValue(Boolean object)
				{
					var label = getString(EnumStringUtil.getPropertyString(bezwaarMomentModel.getObject().getManier()));

					if (Boolean.TRUE.equals(object))
					{
						label += " – handtekening vergeten";
					}
					return label;
				}
			});
		bezwaarAanvragenRadioChoice.setPrefix("<label class=\"radio\">");
		bezwaarAanvragenRadioChoice.setSuffix("</label>");
		container.add(bezwaarAanvragenRadioChoice);
		return container;
	}

	@Override
	public void validate()
	{
		BezwaarMoment bezwaar = bezwaarMomentModel.getObject();
		if (ClientContactManier.DIRECT.equals(bezwaar.getManier()))
		{
			if (bezwaarService.isErEenBezwaarMetType(wrappers, BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER) && !heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN))
			{
				LOG.info("Client Dossier kan niet worden verwijderd vanwege rechten. client(id: " + bezwaar.getClient().getId() + ")");
				error(getString("error.recht.dossierverwijderen"));
			}
			if (bezwaarService.isErEenBezwaarMetType(wrappers, BezwaarType.GEEN_OPNAME_UIT_BPR) && !heeftRechtVoor(Recht.GEBRUIKER_BEZWAAR_BRP))
			{
				LOG.info("Bezwaar BRP kan niet worden ingediend vanwege rechten. client(id: " + bezwaar.getClient().getId() + ")");
				error(getString("error.recht.bezwaarbrp"));
			}
			if (files.getObject().size() != 1)
			{
				error(getString("error.een.bestand.uploaden.bezwaarformulier"));
			}
			if (!bezwarenGewijzigd())
			{
				error(getString("error.bezwaar.niet.gewijzigd"));
			}
		}
	}

	private boolean bezwarenGewijzigd()
	{
		BezwaarMoment laatsteVoltooideBezwaarMoment = clientModel.getObject().getLaatstVoltooideBezwaarMoment();

		return bezwaarService.bezwarenGewijzigd(laatsteVoltooideBezwaarMoment, wrappers, null);
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		var meldingen = super.getOpslaanMeldingen();
		var bezwaar = bezwaarMomentModel.getObject();
		if (ClientContactManier.DIRECT.equals(bezwaar.getManier()))
		{
			stelFileVeilig();
			if ((bezwaarService.isErEenBezwaarMetType(wrappers, BezwaarType.VERZOEK_TOT_VERWIJDERING_DOSSIER) && heeftRechtVoor(Recht.CLIENT_DOSSIER_VERWIJDEREN))
				|| bezwaarService.isErEenBezwaarMetType(wrappers, BezwaarType.GEEN_OPNAME_UIT_BPR) && heeftRechtVoor(Recht.GEBRUIKER_BEZWAAR_BRP))
			{
				meldingen.add(getString("gebruik.gegevens.waarschuwing"));
			}

		}
		return meldingen;
	}

	private void stelFileVeilig()
	{
		try
		{
			if (files.getObject() != null)
			{
				FileUpload upload = files.getObject().get(0);
				File definitieFile = upload.writeToTempFile();

				UploadDocument document = new UploadDocument();
				document.setActief(Boolean.TRUE);
				document.setContentType(upload.getContentType());
				document.setFile(definitieFile);
				document.setNaam(upload.getClientFileName());
				this.document = ModelUtil.cModel(document);
			}
		}
		catch (Exception e)
		{
			LOG.error("Fout bij uploaden van een bezwaaruploaden naar tmp directory: ", e);
			error(getString("error.onbekend"));
		}
	}

	private boolean heeftRechtVoor(Recht recht)
	{
		InstellingGebruiker ingelogdeGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		return autorisatieService.getActieVoorMedewerker(ingelogdeGebruiker, null, recht) != null;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> objecten = super.getOpslaanObjecten();
		BezwaarMoment bezwaarMoment = bezwaarMomentModel.getObject();
		if (ClientContactManier.DIRECT.equals(bezwaarMoment.getManier()))
		{
			try
			{
				Client client = bezwaarMoment.getClient();
				client.getBezwaarMomenten().size(); 
				UploadDocument document = this.document.getObject();
				uploadDocumentService.saveOrUpdate(document, FileStoreLocation.BEZWAAR, client.getId());
				bezwaarMoment.setBezwaarBrief(document);

				bezwaarMoment.setStatus(AanvraagBriefStatus.BRIEF);
				bezwaarMoment.setStatusDatum(currentDateSupplier.getDate());
				hibernateService.saveOrUpdateAll(bezwaarMoment);
			}
			catch (IOException | IllegalStateException e)
			{
				LOG.error("Bezwaar kon niet per direct worden voltooid!");
			}
		}

		objecten.put(ExtraOpslaanKey.BEZWAAR, ModelProxyHelper.deproxy(bezwaarMomentModel.getObject()));
		objecten.put(ExtraOpslaanKey.BEZWAAR_WRAPPERS, wrappers);
		objecten.put(ExtraOpslaanKey.BEZWAAR_VRAGEN_OM_HANDTEKENING, bezwaarAanvragenMetHandtekeningHerinnering.getObject());
		return objecten;
	}
}
