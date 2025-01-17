package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.bezwaar;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.io.File;
import java.util.List;

import nl.rivm.screenit.comparator.BriefCreatieDatumComparator;
import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.bezwaar.tekst.BezwaarTekstPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.DocumentVervangenPanel;
import nl.rivm.screenit.model.BezwaarMoment;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.BezwaarService;
import nl.rivm.screenit.service.BriefHerdrukkenService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.BriefUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.FilenameUtils;
import org.apache.shiro.util.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public abstract class BezwaarInzienPopupPanel extends GenericPanel<BezwaarMoment>
{

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	private BezwaarService bezwaarService;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private BriefHerdrukkenService briefHerdrukkenService;

	private IModel<UploadDocument> upload;

	private WebMarkupContainer uploadForm;

	private final IModel<List<FileUpload>> files = new ListModel<>();

	public BezwaarInzienPopupPanel(String id, IModel<BezwaarMoment> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new Label("wijzeAfmelding", getModelObject().getBezwaarBrief() == null ? "Clientportaal" : "Infolijn"));
		WebMarkupContainer verstuurdFormulierContainer = new WebMarkupContainer("formulierVerstuurdContainer");
		add(verstuurdFormulierContainer);
		verstuurdFormulierContainer.add(
			new ListView<>("brievenLijst", BriefOmschrijvingUtil.getBrievenOmschrijvingen(briefService.getBrievenVanBezwaar(getModelObject())))
			{
				@Override
				protected void populateItem(ListItem<String> item)
				{
					String tekst = item.getModelObject();
					item.add(new Label("brief", Model.of(tekst)));
				}
			});

		add(DateLabel.forDatePattern("bezwaarDatum", new PropertyModel<>(getModel(), "bezwaarDatum"), "dd-MM-yyyy").setVisible(getModelObject().getBezwaarDatum() != null));

		addVervangenPanel();

		addButtons();

		add(new BezwaarTekstPanel("bezwaarTekstPanel", getModel()).setVisible(!getModelObject().getBezwaren().isEmpty()));
	}

	private void addButtons()
	{

		var laatsteBrief = getLaatsteBrief();
		upload = ModelUtil.sModel(getModelObject().getBezwaarBrief());
		var magNogmaalsVersturen = upload != null;
		var magDocumentVervangen = ScreenitSession.get().checkPermission(Recht.VERVANGEN_DOCUMENTEN, Actie.AANPASSEN);
		var heeftTegenhoudenRecht = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_BRIEVEN_TEGENHOUDEN, Actie.AANPASSEN);
		var magTegenhouden = heeftTegenhoudenRecht && laatsteBrief != null && !BriefUtil.isTegengehouden(laatsteBrief) && BriefUtil.getMergedBrieven(laatsteBrief) == null;
		var magDoorvoeren = heeftTegenhoudenRecht && BriefUtil.isTegengehouden(laatsteBrief);
		var footnoteWeergeven = magDoorvoeren || magTegenhouden || magNogmaalsVersturen;

		if (upload != null)
		{
			add(new DownloadLink("bezwaarformulierHandImg", new LoadableDetachableModel<>()
			{
				@Override
				protected File load()
				{
					return uploadDocumentService.load(upload.getObject());
				}
			}, "Bezwaarformulier met Handtekening." + FilenameUtils.getExtension(upload.getObject().getNaam())));
		}
		else
		{
			EmptyPanel empty = new EmptyPanel("bezwaarformulierHandImg");
			empty.setVisible(false);
			add(empty);
		}

		add(new AjaxLink<Void>("tegenhouden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.brieftegenhouden"));
				close(target);
			}
		}.setVisible(magTegenhouden));

		add(new AjaxLink<Void>("doorvoeren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				baseBriefService.briefNietMeerTegenhouden(getLaatsteBrief(), ScreenitSession.get().getLoggedInAccount());
				info(getString("info.briefactiveren"));
				close(target);
			}
		}.setVisible(magDoorvoeren));

		add(new AjaxLink<Void>("nogmaalsVersturen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				List<BezwaarBrief> bevestigingsbrieven = briefService.getOorspronkelijkeBevestigingsbrieven(BezwaarInzienPopupPanel.this.getModelObject());
				briefHerdrukkenService.opnieuwAanmaken(bevestigingsbrieven, ScreenitSession.get().getLoggedInAccount());
				info(getString(
					bevestigingsbrieven.size() > 1 ? "info.bezwaar.meerdere.bevestigingsbrieven.nogmaals.verstuurd" : "info.bezwaar.enkele.bevestigingsbrief.nogmaals.verstuurd"));
				close(target);
			}
		}.setVisible(magNogmaalsVersturen));

		add(new AjaxLink<Void>("vervangen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				uploadForm.setVisible(true);
				target.add(uploadForm);
			}
		}.setVisible(magDocumentVervangen));

		add(new Label("footnote", getString("info.laatste.brief.footnote")).setVisible(footnoteWeergeven));
	}

	private void addVervangenPanel()
	{
		uploadForm = new DocumentVervangenPanel("documentVervangen")
		{
			@Override
			protected void vervangDocument(UploadDocument uploadDocument, AjaxRequestTarget target)
			{
				if (bezwaarService.bezwaarDocumentenVervangen(uploadDocument, getModelObject(), upload.getObject(), ScreenitSession.get().getLoggedInAccount()))
				{
					info(getString("info.vervangendocument"));
					close(target);
				}
				else
				{
					error(getString("error.onbekend"));
				}
			}
		};
		uploadForm.setVisible(false);
		uploadForm.setOutputMarkupId(true);
		uploadForm.setOutputMarkupPlaceholderTag(true);
		add(uploadForm);
	}

	private BezwaarBrief getLaatsteBrief()
	{
		List<BezwaarBrief> brieven = briefService.getBrievenVanBezwaar(getModelObject());
		brieven.sort(new BriefCreatieDatumComparator().reversed());
		if (!CollectionUtils.isEmpty(brieven))
		{
			return brieven.get(0);
		}
		return null;
	}

	protected abstract void close(AjaxRequestTarget target);

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(upload);
		ModelUtil.nullSafeDetach(files);
	}
}
