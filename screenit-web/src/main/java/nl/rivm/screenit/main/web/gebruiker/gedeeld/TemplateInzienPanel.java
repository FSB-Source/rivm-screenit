package nl.rivm.screenit.main.web.gebruiker.gedeeld;

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
import java.io.FileInputStream;
import java.util.Date;

import javax.annotation.CheckForNull;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.util.GebeurtenisUtil;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.util.BriefUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import com.aspose.words.Document;

@Slf4j
public class TemplateInzienPanel extends GenericPanel<Brief>
{
	private BootstrapDialog pdfDialog = new BootstrapDialog("pdfDialog");

	@SpringBean
	private BaseBriefService baseBriefService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	public TemplateInzienPanel(String id, IModel<Brief> model)
	{
		super(id, model);

	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		Date datum = BriefUtil.geefDatumVoorGebeurtenisoverzicht(getModelObject());
		maakBriefInzienContent(getModelObject(), datum);

	}

	private void maakBriefInzienContent(Brief brief, Date datum)
	{
		add(pdfDialog);
		WebMarkupContainer inzienContainer = new WebMarkupContainer("inzienContainer");
		inzienContainer.setOutputMarkupId(true);

		WebMarkupContainer inzienGroep = new WebMarkupContainer("inzienGroep");

		Label inzienMsg;
		if (brief.getBriefDefinitie() != null)
		{
			inzienMsg = new Label("inzienMelding");
			inzienMsg.setVisible(false);

			final File file = uploadDocumentService.load(brief.getBriefDefinitie().getDocument());
			inzienGroep.add(new IndicatingAjaxLink<Void>("inzien")
			{
				@Override
				public void onClick(AjaxRequestTarget ajaxRequestTarget)
				{
					final Document document = genereerAsposeDocument(file);
					if (document != null)
					{
						showPdf(ajaxRequestTarget, document);
					}
				}
			});
		}
		else
		{
			inzienMsg = new Label("inzienMelding", "Gebruikte template niet beschikbaar, want deze brief is " +
				(brief.getTemplateNaam() == null ? "nog niet verzonden" : "verzonden voordat ScreenIT de templates van de verzonden brieven ging bewaren, of nog niet verzonden"));
			inzienGroep.setVisible(false);
		}
		WebMarkupContainer inzienMeldingGroup = new WebMarkupContainer("inzienMeldingGroup");
		inzienMeldingGroup.setOutputMarkupId(true);
		inzienMeldingGroup.add(inzienMsg);
		inzienMeldingGroup.setVisible(inzienMsg.isVisible());
		inzienContainer.add(inzienMeldingGroup);

		GebeurtenisUtil.voegBriefTypeOfNaamBriefToe(inzienGroep, brief);

		inzienGroep.add(DateLabel.forDatePattern("brief.creatieDatum", Model.of(datum), "dd-MM-yyyy"));
		inzienGroep.setOutputMarkupId(true);

		inzienContainer.add(inzienGroep);
		add(inzienContainer);
	}

	private boolean showPdf(AjaxRequestTarget target, Document document)
	{
		try
		{

			target.appendJavaScript("$('.modal.fade.in').not('#sessieVerlopenDialog').addClass('previousDialog').modal('hide');");

			pdfDialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, baseBriefService.genereerPdf(document, "brieftemplate_inzien", false)));
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return true;
		}
		return false;
	}

	@CheckForNull
	private Document genereerAsposeDocument(File file)
	{
		try
		{
			FileInputStream stream = new FileInputStream(file);
			return new Document(stream);
		}
		catch (Exception e)
		{
			LOG.error(e.getMessage());
			return null;
		}
	}
}
